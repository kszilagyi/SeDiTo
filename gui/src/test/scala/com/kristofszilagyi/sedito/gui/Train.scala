package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.aligner.{AccessibleScaler, FeaturesWithResults, PathAndSamples, Samples}
import org.log4s.getLogger
import smile.classification
import smile.classification.NeuralNetwork
import smile.classification.NeuralNetwork.{ActivationFunction, ErrorFunction}
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts
import com.kristofszilagyi.sedito.gui.TrainAndDiff.PerformanceMetrics
import smile.feature.Scaler
import smile.validation.f1

object Train {
  val trainingRatio = 0.8

  def calcNumOfAttributes(metrics: List[Traversable[FeaturesWithResults]]): Int = {
    @SuppressWarnings(Array(Warts.OptionPartial))
    val nonEmpty = metrics.find(_.nonEmpty).get
    val num = nonEmpty.head.features.doubles.length
    num
  }

  @SuppressWarnings(Array(Warts.ToString))
  private def performanceMetrics(files: List[PathAndSamples], scaler: Scaler,
                         classifier: NeuralNetwork, numOfAttributes: Int, idxesToExclude: Set[Int]): List[(String, PerformanceMetrics)] = {
    files.map { case Pass1PathAndSamples(path, singleTest) =>
      val singleDataSet = toAttributeDataSet(singleTest.featuresWithResults, numOfAttributes, idxesToExclude)
      val singleTestX = scaler.transform(singleDataSet.x())
      val singleTestY = singleDataSet.labels()
      val singlePred = singleTestX.map(classifier.predict)
      val f1Score = f1(singleTestY, singlePred)
      val fp = countFP(singleTestY, singlePred)
      val fn = countFN(singleTestY, singlePred)
      val tp = countTP(singleTestY, singlePred)
      val selPos = countSelPos(singleTestY, singlePred)
      val expectedPos = countExpectedPos(singleTestY, singlePred)
      path.getFileName.toString.padTo(34, " ").mkString -> PerformanceMetrics(f1Score, fn = fn,
        fp = fp, tp = tp, selPos = selPos, expectedPos = expectedPos,sampleSize = singleTestY.length)
    }.sortBy(_._2.f1)
  }

  private def toAttributeDataSet(featuresWithResults: Traversable[FeaturesWithResults], numOfAttributes: Int, excludedIdxes: Set[Int]) = {
    val idxesToKeep = (0 until numOfAttributes).filterNot(excludedIdxes.contains)
    val attributes = idxesToKeep.map { name =>
      new NumericAttribute(name.toString)
    }
    val attributeDataset = new AttributeDataset("matches", attributes.toArray, new NominalAttribute("doesMatch"))
    featuresWithResults.foreach { m =>
      val doubles = m.features.doubles
      assert(numOfAttributes ==== doubles.length, s"$numOfAttributes != ${doubles.length}")
      val valuesToKeep = idxesToKeep.map{idx => doubles(idx)}.toArray
      attributeDataset.add(new attributeDataset.Row(valuesToKeep, if (m.matching) 1.0 else 0.0))
    }
    attributeDataset
  }

  def countFP(truth: Array[Int], prediction: Array[Int]): Int = {
    truth.zip(prediction).count{ case (t, p) => t ==== 0 && p ==== 1 }
  }

  def countFN(truth: Array[Int], prediction: Array[Int]): Int = {
    truth.zip(prediction).count{ case (t, p) => t ==== 1 && p ==== 0 }
  }

  def countTP(truth: Array[Int], prediction: Array[Int]): Int = {
    truth.zip(prediction).count{ case (t, p) => t ==== 1 && p ==== 1 }
  }

  def countSelPos(truth: Array[Int], prediction: Array[Int]): Int = {
    truth.zip(prediction).count{ case (_, p) => p ==== 1 }
  }

  def countExpectedPos(truth: Array[Int], prediction: Array[Int]): Int = {
    truth.zip(prediction).count{ case (t, _) => t ==== 1 }
  }

  def logBasicStats(nestedTraining: List[Samples],
                    nestedTest: List[Samples]): Unit = {
    val training = nestedTraining.flatMap(_.featuresWithResults)
    val test = nestedTest.flatMap(_.featuresWithResults)
    logger.info(s"Number of features: ${training.head.features.doubles.length}")
    logger.info(s"Training size: ${training.size}")
    logger.info(s"Test size: ${test.size}")
    logger.info(s"1s in training: ${training.count(_.matching)}")
    logger.info(s"0s in training: ${training.count(_.matching ==== false)}")
    logger.info(s"1s in test: ${test.count(_.matching)}")
    logger.info(s"0s in test: ${test.count(_.matching ==== false)}")
  }

  def generateClassifier(nestedTraining: List[Samples], nestedTest: List[Samples],
                         numOfAttributes: Int, idxesToExclude: Set[Int], hiddenLayerSize: Int): (NeuralNetwork, AccessibleScaler, TrainingData) = {
    val training = nestedTraining.flatMap(_.featuresWithResults)
    val test = nestedTest.flatMap(_.featuresWithResults)

    val trainingSet = toAttributeDataSet(training, numOfAttributes, idxesToExclude)
    val scaler = new AccessibleScaler(true)
    scaler.learn(trainingSet.attributes(), trainingSet.x())
    val transformedTrainingSet = scaler.transform(trainingSet.x())
    val trainingY = trainingSet.labels()
    val classifier = classification.mlp(transformedTrainingSet, trainingY, Array(numOfAttributes - idxesToExclude.size, hiddenLayerSize, 1), ErrorFunction.CROSS_ENTROPY, ActivationFunction.LOGISTIC_SIGMOID)
    logger.info("Classifier created, creating predictions.")
    val testSet = toAttributeDataSet(test, numOfAttributes, idxesToExclude)
    val testX = scaler.transform(testSet.x())
    val testY = testSet.labels()
    val trainingPred = transformedTrainingSet.map(classifier.predict)
    val testPred = testX.map(classifier.predict)

    val trainingData = TrainingData(
      training = new YAndPred(trainingY, trainingPred),
      test = new YAndPred(testY, testPred)
    )
    (classifier, scaler, trainingData)
  }

  private val logger = getLogger

  def train(training: List[PathAndSamples], test: List[PathAndSamples], logStats: Boolean, hiddenLayerSize: Int) = {
    val samples = training ++ test
    val featuresWithResults = samples.map(_.samples.featuresWithResults)
    val numOfAttributes = calcNumOfAttributes(featuresWithResults)

    if (logStats) {
      logBasicStats(training.map(_.samples), nestedTest = test.map(_.samples))
      logger.info("Starting training")
    }

    val (classifier, scaler, trainingData) = generateClassifier(nestedTraining = training.map(_.samples),
      nestedTest = test.map(_.samples), numOfAttributes, idxesToExclude = Set.empty, hiddenLayerSize = hiddenLayerSize)

    val f1s = trainingData.f1s
    if (logStats) {
      trainingData.printDetailedStats(f1s)
      val trainingMetrics = performanceMetrics(training, scaler, classifier, numOfAttributes, idxesToExclude = Set.empty)

      logger.info("Training f1s: \n" + trainingMetrics.mkString("\n"))

      val testMetrics = performanceMetrics(test, scaler, classifier, numOfAttributes, idxesToExclude = Set.empty)
      logger.info("Test f1s: \n" + testMetrics.mkString("\n"))
    }
    (classifier, scaler, f1s)
  }
}