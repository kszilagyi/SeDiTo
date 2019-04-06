package com.kristofszilagyi.sedito.gui

import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.Pass1FeatureCalculator._
import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, WordMatch}
import com.kristofszilagyi.sedito.gui.Main.{firstPhaseClassifierName, firstPhaseScalerName}
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
import scala.util.{Failure, Success}


final case class Pass1FeaturesWithResults(features: Pass1Features, matching: Boolean) extends FeaturesWithResults
final case class Pass1Samples(featuresWithResults: Traversable[Pass1FeaturesWithResults]) extends Samples

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

  private def readSingleDataSetAndMeasureFeatures(testDir: Path, reverse: Boolean) = {
    val testCase = readTestCase(testDir)
    val metrics =
      if (!reverse) calcAlignerFeatures(testCase.left, testCase.right)
      else calcAlignerFeatures(testCase.right, testCase.left)

    val matches =
      if (!reverse) testCase.wordAlignment.matches.toSeq
      else testCase.wordAlignment.matches.toSeq.map(_.reverse)

    val matchesSet = matches.toSet
    discard(assert(matches.size ==== matchesSet.size))
    val metricsWithResult = metrics.map { m =>
      val potentialMatch = WordMatch(m.leftWord, m.rightWord)()
      Pass1FeaturesWithResults(m, matching = matchesSet.contains(potentialMatch))
    }

    Pass1Samples(metricsWithResult)
  }

  def testDirs: Seq[Path] = {
    val parentDir = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p)).sorted
    }
  }

  private def readSingleOneWay(testDir: Path, reverse: Boolean) = {
    val samples = readSingleDataSetAndMeasureFeatures(testDir, reverse = reverse)
    if (samples.featuresWithResults.nonEmpty) {
      val metricsNumInSamples = calcNumOfAttributes(List(samples.featuresWithResults))
      val columnCount = Pass1Features.columnNames.size
      assert(metricsNumInSamples ==== columnCount, s"$metricsNumInSamples != $columnCount")
    }
    val path =
      if (!reverse) testDir
      else Paths.get(testDir.toString + "_reverse")
    Pass1PathAndSamples(path, samples)
  }

  def readDataSetAndMeasureFeatures(): List[Pass1PathAndSamples] = {

    val metrics = testDirs.par.flatMap { testDir =>
      List(readSingleOneWay(testDir, reverse = false), readSingleOneWay(testDir, reverse = true))
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
  def order[T <: WithPath](samplesByPath: List[T]): List[T] = {
    samplesByPath.sortBy(_.path.getFileName)
  }

  private val logger = getLogger
  val hiddenLayerSize = 50

  private def crossValidate(orderedSamples: List[Pass1PathAndSamples]) = {
    val trainingSize = (orderedSamples.size * trainingRatio).toInt
    val testSize = orderedSamples.size - trainingSize
    (0 to 3) map { i => //4-5 is the default (outside this method), that's why we omit here
      val testStart = i * testSize
      val testEnd = testStart + testSize
      logger.info(s"testStart: $testStart, testEnd: $testEnd")
      val training = orderedSamples.slice(0, testStart) ++ orderedSamples.drop(testEnd)
      val test = orderedSamples.slice(testStart, testEnd)
      assert(training.size ==== trainingSize)
      assert(test.size ==== testSize)
      Future { train(training, test, logStats = false, hiddenLayerSize)._3 }
    }
  }


  def main(args: Array[String]) {
    logger.info("Start")
    val start = Instant.now()
    val orderedSamples = order(readDataSetAndMeasureFeatures())
    val crossValidates = crossValidate(orderedSamples)
    val trainingSize = (orderedSamples.size * trainingRatio).toInt
    val testSize = orderedSamples.size - trainingSize

    val (training, test) = orderedSamples.splitAt(testSize * 4)
    logger.info(s"trainingSize: ${training.size}, testSize: ${test.size}")
    val (classifier, scaler, mainF1s) = train(training, test, logStats = true, hiddenLayerSize)
    val firstPhaseClassifierPath = s"aligner/src/main/resources/$firstPhaseClassifierName"
    val firstPhaseScalerPath = s"aligner/src/main/resources/$firstPhaseScalerName"
    write.xstream(classifier, firstPhaseClassifierPath)
    write.xstream(scaler, firstPhaseScalerPath)
    val duration = Duration.between(start, Instant.now())
    val crossF1s = Await.result(Future.sequence(crossValidates), 10.minutes)
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis / 1000 - duration.toMinutes * 60} seconds")

    logger.info(s"Main f1s: ${mainF1s.all}")
    logger.info(s"Cross f1s: ${crossF1s.map(_.all).mkString("\n")}")
    val allF1s = mainF1s +: crossF1s
    val avg = allF1s.reduce(_ + _) / allF1s.size
    logger.info(s"Avg: ${avg.f1s}")
  }
}

object Diff extends App {
  Application.launch(classOf[ShowOne])
}