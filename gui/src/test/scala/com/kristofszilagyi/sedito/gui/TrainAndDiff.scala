package com.kristofszilagyi.sedito.gui

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.aligner.{AccessibleScaler, Aligner, MetricCalculator}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, Warts, WordMatch}
import com.kristofszilagyi.sedito.gui.TrainAndDiff._
import javafx.application.Application
import javafx.stage.Stage
import org.log4s.getLogger
import smile.classification
import smile.classification.NeuralNetwork.{ActivationFunction, ErrorFunction}
import smile.classification.{NeuralNetwork, SoftClassifier}
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.feature.Scaler
import smile.validation._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final case class MetricsWithResults(metrics: Metrics, matching: Boolean)
final case class Samples(lostPositives: Int, metricsWithResults: Traversable[MetricsWithResults])

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
    val metrics = MetricCalculator.calcAlignerMetrics(testCase.left, testCase.right)

    val matches = testCase.wordAlignment.matches.toSeq
    val matchesSet = matches.toSet
    discard(assert(matches.size ==== matchesSet.size))
    val metricsWithResult = metrics.map { m =>
      val potentialMatch = WordMatch(m.leftWord, m.rightWord)()
      MetricsWithResults(m, matching = matchesSet.contains(potentialMatch))
    }

    val lost = matches.size - metricsWithResult.count(_.matching)
    Samples(lost, metricsWithResult)
  }

  def testDirs: Seq[Path] = {
    val parentDir = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p)).sorted
    }
  }

  def readDataSetAndMeasureMetrics() = {

    val metrics = testDirs.par.map { testDir =>
      val samples = readSingleDataSetAndMeasureMetrics(testDir)
      if (samples.metricsWithResults.nonEmpty) {
        val metricsNumInSamples = calcNumOfAttributes(List(samples.metricsWithResults))
        val columnCount = Metrics.columnNames.size
        assert(metricsNumInSamples ==== columnCount, s"$metricsNumInSamples != $columnCount")
      }
      testDir -> samples
    }
    metrics.seq.toList
  }


  private def toAttributeDataSet(metrics: Traversable[MetricsWithResults], numOfAttributes: Int, excludedIdxes: Set[Int]) = {
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

  def logBasicStats(nestedTraining: List[Samples],
                    nestedTest: List[Samples]): Unit = {
    val training = nestedTraining.flatMap(_.metricsWithResults)
    val test = nestedTest.flatMap(_.metricsWithResults)
    logger.info(s"Training size: ${training.size}")
    logger.info(s"Test size: ${test.size}")
    logger.info(s"1s in training: ${training.count(_.matching)}")
    logger.info(s"0s in training: ${training.count(_.matching ==== false)}")
    logger.info(s"1s in test: ${test.count(_.matching)}")
    logger.info(s"0s in test: ${test.count(_.matching ==== false)}")
  }

  //todo try with more neurons
  def generateClassifier(nestedTraining: List[Samples], nestedTest: List[Samples],
                         numOfAttributes: Int, idxesToExclude: Set[Int]): (NeuralNetwork, AccessibleScaler, TrainingData) = {
    val training = nestedTraining.flatMap(_.metricsWithResults)
    val test = nestedTest.flatMap(_.metricsWithResults)

    val trainingSet = toAttributeDataSet(training, numOfAttributes, idxesToExclude)
    val scaler = new AccessibleScaler(true)
    scaler.learn(trainingSet.attributes(), trainingSet.x())
    val transformedTrainingSet = scaler.transform(trainingSet.x())
    val trainingY = trainingSet.labels()
    val classifier = classification.mlp(transformedTrainingSet, trainingY, Array(numOfAttributes - idxesToExclude.size, 5, 1), ErrorFunction.CROSS_ENTROPY, ActivationFunction.LOGISTIC_SIGMOID)
    logger.info("Classifier created, creating predictions.")
    val testSet = toAttributeDataSet(test, numOfAttributes, idxesToExclude)
    val testX = scaler.transform(testSet.x())
    val testY = testSet.labels()
    val trainingPred = transformedTrainingSet.map(classifier.predict)
    val testPred = testX.map(classifier.predict)

    val lostPositivesInTraining = nestedTraining.map(_.lostPositives).sum
    val lostPositivesInTest = nestedTest.map(_.lostPositives).sum

    val trainingData = TrainingData(
      training = new YAndPred(trainingY, trainingPred),
      test = new YAndPred(testY, testPred),
      lostPositivesInTraining = lostPositivesInTraining,
      lostPositivesInTest = lostPositivesInTest
    )
    (classifier, scaler, trainingData)
  }

  private def displayTestCase(testCase: TestCase, classifier: SoftClassifier[Array[Double]], scaler: Scaler) = {
    val calculatedAlignment = new Aligner(classifier, scaler).align(testCase.left, testCase.right)
    logger.info("Aligning finished")
    val expected = new MainWindow()
    expected.setTitle("Expected")
    val unambiguousWordAlignment = testCase.wordAlignment.toUnambigous
    expected.setContent(testCase.left, testCase.right, unambiguousWordAlignment)
    val actual = new MainWindow()
    actual.setTitle("Actual")
    actual.setContent(testCase.left, testCase.right, calculatedAlignment)
    logger.info("opening finished")
  }

  def calcNumOfAttributes(metrics: List[Traversable[MetricsWithResults]]): Int = {
    @SuppressWarnings(Array(Warts.OptionPartial))
    val nonEmpty = metrics.find(_.nonEmpty).get
    val num = nonEmpty.head.metrics.doubles.length
    num
  }



  final class ShowOne extends Application {
    def start(stage: Stage): Unit = {

      val (classifier, scaler) = Main.loadAI()
//      val testCase = readTestCase(Paths.get("//home/szkster/IdeaProjects/SeDiTo/common/target/" +
//        "scala-2.12/test-classes/algorithm_tests/full_tests/modelexactlysame" +
//        ""))
      val testCase = readTestCase(Paths.get("//home/szkster/IdeaProjects/SeDiTo/common/target/" +
        "scala-2.12/test-classes/algorithm_tests/too_slow/metricscalculatortest" +
        ""))

      displayTestCase(testCase, classifier, scaler)
    }
  }

  final case class PerformanceMetrics(f1: Double, lostPositives: Int, fn: Int, fp: Int, tp: Int, selPos: Int, expectedPos: Int, sampleSize: Int) {
    override def toString: String = f"f1: $f1%.3f, tp: $tp%4d, fp: $fp%2d, fn: $fn%2d, selPos: $selPos%4d, " +
      f"expectedPos: $expectedPos%4d, lost positives: $lostPositives, sample size: $sampleSize"
  }

  @SuppressWarnings(Array(Warts.ToString))
  def performanceMetrics(files: List[(Path, Samples)], scaler: Scaler,
                         classifier: NeuralNetwork, numOfAttributes: Int, idxesToExclude: Set[Int]) = {
    files.map { case (path, singleTest) =>
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
      path.getFileName.toString.padTo(34, " ").mkString -> PerformanceMetrics(f1Score, singleTest.lostPositives, fn = fn,
        fp = fp, tp = tp, selPos = selPos, expectedPos = expectedPos,sampleSize = singleTestY.length)
    }.sortBy(_._2.f1)
  }

}
object Train {
  private val logger = getLogger

  def train(training: List[(Path, Samples)], test: List[(Path, Samples)], logStats: Boolean): (NeuralNetwork, AccessibleScaler) = {
    val samples = training ++ test
    val metricsWithResults = samples.map(_._2.metricsWithResults)
    val numOfAttributes = calcNumOfAttributes(metricsWithResults)

    if (logStats) {
      logBasicStats(training.map(_._2), nestedTest = test.map(_._2))
      logger.info("Starting training")
    }

    val (classifier, scaler, trainingData) = generateClassifier(nestedTraining = training.map(_._2),
      nestedTest = test.map(_._2), numOfAttributes, idxesToExclude = Set.empty)

    if (logStats) {
      trainingData.printDetailedStats()
      val trainingMetrics = performanceMetrics(training, scaler, classifier, numOfAttributes, idxesToExclude = Set.empty)

      logger.info("Training f1s: \n" + trainingMetrics.mkString("\n"))

      val testMetrics = performanceMetrics(test, scaler, classifier, numOfAttributes, idxesToExclude = Set.empty)
      logger.info("Test f1s: \n" + testMetrics.mkString("\n"))
    }
    (classifier, scaler)
  }

  // This is necessary to avoid reading from xml (performance)
  private def writeScaler(scaler: AccessibleScaler, path: Path)  {
    val text =
      s"""package com.kristofszilagyi.sedito.aligner;
         |import smile.feature.Scaler;
         |
         |final public class HardcodedScaler extends Scaler {
         |  public HardcodedScaler() {
         |    super(true);
         |    lo = new double[]{${scaler.getLo.mkString(", ")}};
         |    hi = new double[]{${scaler.getHi.mkString(", ")}};
         |  }
         |}
       """.stripMargin
    discard(Files.write(path, text.getBytes(StandardCharsets.UTF_8)))
  }

  private def toString(dd: Array[Array[Double]]): String = {
    "new double[][]{{" + dd.map(_.mkString(", ")).mkString("}, {") + "}}"
  }
  private def writeNN(nn: NeuralNetwork, path: Path)  {
    val text =
      s"""package com.kristofszilagyi.sedito.aligner;
         |import smile.classification.LoadableNeuralNetwork;
         |
         |final public class HardcodedNeuralNetwork {
         |  public static final LoadableNeuralNetwork nn = new LoadableNeuralNetwork(${Metrics.columnNames.length}, new double[][][]{
         |    ${toString(nn.getWeight(1))},
         |    ${toString(nn.getWeight(2))}
         |  });
         |}
       """.stripMargin
    discard(Files.write(path, text.getBytes(StandardCharsets.UTF_8)))
  }

  def main(args: Array[String]) {
    logger.info("Start")
    val start = Instant.now()
    val samples = readDataSetAndMeasureMetrics()
    val (training, test) = samples.splitAt(samples.size / 2)
    val (classifier, scaler) = train(training, test, logStats = true)
    writeNN(classifier, Paths.get("aligner/src/main/scala/com/kristofszilagyi/sedito/aligner/HardcodedNeuralNetwork.java"))
    writeScaler(scaler, Paths.get("aligner/src/main/scala/com/kristofszilagyi/sedito/aligner/HardcodedScaler.java"))
    val duration = Duration.between(start, Instant.now())
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis / 1000 - duration.toMinutes * 60} seconds")
  }
}



object Diff extends App {
  Application.launch(classOf[ShowOne])
}
