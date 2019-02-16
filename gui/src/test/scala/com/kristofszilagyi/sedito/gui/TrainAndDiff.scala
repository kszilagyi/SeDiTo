package com.kristofszilagyi.sedito.gui

import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Metrics
import com.kristofszilagyi.sedito.aligner.{AccessibleScaler, FirstPassAligner, Pass1MetricCalculator}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, Warts, WordMatch}
import com.kristofszilagyi.sedito.gui.TrainAndDiff._
import javafx.application.Application
import javafx.stage.Stage
import org.log4s.getLogger
import smile.{classification, write}
import smile.classification.NeuralNetwork.{ActivationFunction, ErrorFunction}
import smile.classification.{NeuralNetwork, SoftClassifier}
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.feature.Scaler
import smile.validation._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationDouble
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}

final case class Pass1MetricsWithResults(metrics: Pass1Metrics, matching: Boolean)
final case class Pass1Samples(metricsWithResults: Traversable[Pass1MetricsWithResults])

final case class Pass1PathAndSamples(path: Path, samples: Pass1Samples)

object TrainAndDiff {
  private val logger = getLogger

  def readTestCase(testDir: Path): TestCase = {
    TestCase.open(testDir) match {
      case Failure(exception) =>
        println(s"$testDir -> ${exception.getMessage}")
        sys.exit(1)
      case Success(testCase) => testCase
    }
  }

  private def readSingleDataSetAndMeasureMetrics(testDir: Path) = {
    val testCase = readTestCase(testDir)
    val metrics = Pass1MetricCalculator.calcAlignerMetrics(testCase.left, testCase.right)

    val matches = testCase.wordAlignment.matches.toSeq
    val matchesSet = matches.toSet
    discard(assert(matches.size ==== matchesSet.size))
    val metricsWithResult = metrics.map { m =>
      val potentialMatch = WordMatch(m.leftWord, m.rightWord)()
      Pass1MetricsWithResults(m, matching = matchesSet.contains(potentialMatch))
    }

    Pass1Samples(metricsWithResult)
  }

  def testDirs: Seq[Path] = {
    val parentDir = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p)).sorted
    }
  }

  def readDataSetAndMeasureMetrics(): List[Pass1PathAndSamples] = {

    val metrics = testDirs.par.map { testDir =>
      val samples = readSingleDataSetAndMeasureMetrics(testDir)
      if (samples.metricsWithResults.nonEmpty) {
        val metricsNumInSamples = calcNumOfAttributes(List(samples.metricsWithResults))
        val columnCount = Pass1Metrics.columnNames.size
        assert(metricsNumInSamples ==== columnCount, s"$metricsNumInSamples != $columnCount")
      }
      Pass1PathAndSamples(testDir, samples)
    }
    metrics.seq.toList
  }


  private def toAttributeDataSet(metrics: Traversable[Pass1MetricsWithResults], numOfAttributes: Int, excludedIdxes: Set[Int]) = {
    val idxesToKeep = (0 until numOfAttributes).filterNot(excludedIdxes.contains)
    val attributes = idxesToKeep.map { name =>
      new NumericAttribute(name.toString)
    }
    val attributeDataset = new AttributeDataset("matches", attributes.toArray, new NominalAttribute("doesMatch"))
    metrics.foreach { m =>
      val doubles = m.metrics.doubles
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

  def logBasicStats(nestedTraining: List[Pass1Samples],
                    nestedTest: List[Pass1Samples]): Unit = {
    val training = nestedTraining.flatMap(_.metricsWithResults)
    val test = nestedTest.flatMap(_.metricsWithResults)
    logger.info(s"Training size: ${training.size}")
    logger.info(s"Test size: ${test.size}")
    logger.info(s"1s in training: ${training.count(_.matching)}")
    logger.info(s"0s in training: ${training.count(_.matching ==== false)}")
    logger.info(s"1s in test: ${test.count(_.matching)}")
    logger.info(s"0s in test: ${test.count(_.matching ==== false)}")
  }

  def generateClassifier(nestedTraining: List[Pass1Samples], nestedTest: List[Pass1Samples],
                         numOfAttributes: Int, idxesToExclude: Set[Int]): (NeuralNetwork, AccessibleScaler, TrainingData) = {
    val training = nestedTraining.flatMap(_.metricsWithResults)
    val test = nestedTest.flatMap(_.metricsWithResults)

    val trainingSet = toAttributeDataSet(training, numOfAttributes, idxesToExclude)
    val scaler = new AccessibleScaler(true)
    scaler.learn(trainingSet.attributes(), trainingSet.x())
    val transformedTrainingSet = scaler.transform(trainingSet.x())
    val trainingY = trainingSet.labels()
    val classifier = classification.mlp(transformedTrainingSet, trainingY, Array(numOfAttributes - idxesToExclude.size, 50, 1), ErrorFunction.CROSS_ENTROPY, ActivationFunction.LOGISTIC_SIGMOID)
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

  private def displayTestCase(testCase: TestCase, leftPath: Path, rightPath: Path, classifier: SoftClassifier[Array[Double]], scaler: Scaler): Unit = {
    val calculatedAlignment = new FirstPassAligner(classifier, scaler).align(testCase.left, testCase.right)
    logger.info("Aligning finished")
    val expected = new MainWindow()
    expected.setTitle("Expected")
    val unambiguousWordAlignment = testCase.wordAlignment.toUnambiguous
    expected.setContent(testCase.left, testCase.right, leftPath, rightPath, unambiguousWordAlignment)
    val actual = new MainWindow()
    actual.setTitle("Actual")
    actual.setContent(testCase.left, testCase.right, leftPath, rightPath, calculatedAlignment)
    logger.info("opening finished")
  }

  def calcNumOfAttributes(metrics: List[Traversable[Pass1MetricsWithResults]]): Int = {
    @SuppressWarnings(Array(Warts.OptionPartial))
    val nonEmpty = metrics.find(_.nonEmpty).get
    val num = nonEmpty.head.metrics.doubles.length
    num
  }



  final class ShowOne extends Application {
    def start(stage: Stage): Unit = {

      val (classifier, scaler) = Main.loadAI()
      val path = Paths.get("//home/szkster/IdeaProjects/SeDiTo/common/target/" +
        "scala-2.12/test-classes/algorithm_tests/full_tests/test_model_py2")
      val testCase = readTestCase(path)
      val leftPath = TestCase.leftPath(path)
      val rightPath = TestCase.rightPath(path)

      displayTestCase(testCase, leftPath, rightPath, classifier, scaler)
    }
  }

  final case class PerformanceMetrics(f1: Double, fn: Int, fp: Int, tp: Int, selPos: Int, expectedPos: Int, sampleSize: Int) {
    override def toString: String = f"f1: $f1%.3f, tp: $tp%4d, fp: $fp%2d, fn: $fn%2d, selPos: $selPos%4d, " +
      f"expectedPos: $expectedPos%4d, sample size: $sampleSize"
  }

  @SuppressWarnings(Array(Warts.ToString))
  def performanceMetrics(files: List[Pass1PathAndSamples], scaler: Scaler,
                         classifier: NeuralNetwork, numOfAttributes: Int, idxesToExclude: Set[Int]): List[(String, PerformanceMetrics)] = {
    files.map { case Pass1PathAndSamples(path, singleTest) =>
      val singleDataSet = toAttributeDataSet(singleTest.metricsWithResults, numOfAttributes, idxesToExclude)
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

}
object Train1Pass {
  private val logger = getLogger

  def train(training: List[Pass1PathAndSamples], test: List[Pass1PathAndSamples], logStats: Boolean): (NeuralNetwork, AccessibleScaler) = {
    val samples = training ++ test
    val metricsWithResults = samples.map(_.samples.metricsWithResults)
    val numOfAttributes = calcNumOfAttributes(metricsWithResults)

    if (logStats) {
      logBasicStats(training.map(_.samples), nestedTest = test.map(_.samples))
      logger.info("Starting training")
    }

    val (classifier, scaler, trainingData) = generateClassifier(nestedTraining = training.map(_.samples),
      nestedTest = test.map(_.samples), numOfAttributes, idxesToExclude = Set.empty)

    if (logStats) {
      trainingData.printDetailedStats()
      val trainingMetrics = performanceMetrics(training, scaler, classifier, numOfAttributes, idxesToExclude = Set.empty)

      logger.info("Training f1s: \n" + trainingMetrics.mkString("\n"))

      val testMetrics = performanceMetrics(test, scaler, classifier, numOfAttributes, idxesToExclude = Set.empty)
      logger.info("Test f1s: \n" + testMetrics.mkString("\n"))
    }
    (classifier, scaler)
  }

  val trainingRatio = 0.7

  private def crossValidate(samples: List[Pass1PathAndSamples]) = {
    (1 to 3) map { _ =>
      val randomSamples = Random.shuffle(samples)
      val (training, test) = randomSamples.splitAt((samples.size * trainingRatio).toInt)
      Future { train(training, test, logStats = true) }
    }
  }

  def main(args: Array[String]) {
    logger.info("Start")
    val start = Instant.now()
    val samples = readDataSetAndMeasureMetrics()
    val crossValidates = crossValidate(samples)
    val random = new Random(124) //make it repeatable but avoid weird dependence on file structure
    val (training, test) = random.shuffle(samples).splitAt((samples.size * trainingRatio).toInt)
    val (classifier, scaler) = train(training, test, logStats = true)
    write.xstream(classifier, Main.firstPhaseClassifierPath)
    write.xstream(scaler, Main.firstPhaseScalerPath)
    val duration = Duration.between(start, Instant.now())
    discard(Await.ready(Future.sequence(crossValidates), 10.minutes))
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis / 1000 - duration.toMinutes * 60} seconds")
  }
}



object Diff extends App {
  Application.launch(classOf[ShowOne])
}