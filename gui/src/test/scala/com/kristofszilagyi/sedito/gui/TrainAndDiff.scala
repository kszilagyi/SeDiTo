package com.kristofszilagyi.sedito.gui

import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Metrics
import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, WordMatch}
import com.kristofszilagyi.sedito.gui.Train._
import com.kristofszilagyi.sedito.gui.TrainAndDiff._
import javafx.application.Application
import javafx.stage.Stage
import org.log4s.getLogger
import smile.classification.SoftClassifier
import smile.feature.Scaler
import smile.write

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationDouble
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}


final case class Pass1MetricsWithResults(metrics: Pass1Metrics, matching: Boolean) extends MetricsWithResults
final case class Pass1Samples(metricsWithResults: Traversable[Pass1MetricsWithResults]) extends Samples

final case class Pass1PathAndSamples(path: Path, samples: Pass1Samples) extends PathAndSamples

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

  private def displayTestCase(testCase: TestCase, leftPath: Path, rightPath: Path, classifier: SoftClassifier[Array[Double]], scaler: Scaler): Unit = {
    val calculatedAlignment = new Pass1Aligner(classifier, scaler).align(testCase.left, testCase.right)
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



}
object Train1Pass {
  private val logger = getLogger
  val hiddenLayerSize = 50

  private def crossValidate(samples: List[Pass1PathAndSamples]) = {
    (1 to 3) map { _ =>
      val randomSamples = Random.shuffle(samples)
      val (training, test) = randomSamples.splitAt((samples.size * trainingRatio).toInt)
      Future { train(training, test, logStats = true, hiddenLayerSize) }
    }
  }

  def main(args: Array[String]) {
    logger.info("Start")
    val start = Instant.now()
    val samples = readDataSetAndMeasureMetrics()
    val crossValidates = crossValidate(samples)
    val random = new Random(124) //make it repeatable but avoid weird dependence on file structure
    val (training, test) = random.shuffle(samples).splitAt((samples.size * trainingRatio).toInt)
    val (classifier, scaler) = train(training, test, logStats = true, hiddenLayerSize)
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