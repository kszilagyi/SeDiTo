package com.kristofszilagyi.sedito.gui

import java.awt.Color
import java.nio.file.{Files, Path, Paths}

import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.aligner.{Aligner, MetricCalculator}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, Warts, WordMatch}
import com.kristofszilagyi.sedito.gui.PlotData._
import javafx.application.Application
import javafx.stage.Stage
import org.log4s.getLogger
import org.scalatest.FreeSpecLike
import smile.classification.LogisticRegression
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.feature.Scaler
import smile.validation._
import smile.{classification, plot, read, write}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final case class MetricsWithResults(metrics: Metrics, matching: Boolean)

object PlotData {
  private val logger = getLogger

  private def readTestCase(testDir: Path): TestCase = {
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

    metrics.map { m =>
      val potentialMatch = WordMatch(m.leftWord, m.rightWord)
      MetricsWithResults(m, matching = matchesSet.contains(potentialMatch))
    }
  }

  private def readDataSetAndMeasureMetrics() = {
    val parentDir = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    val testDirs = using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p))
    }
    val metrics = testDirs.par.map{ testDir =>
      testDir -> readSingleDataSetAndMeasureMetrics(testDir)
    }
    metrics.seq.toList
  }

  private def toAttributeDataSet(metrics: Traversable[MetricsWithResults]) = {
    val numOfAttributes = 7
    val attributes = (0 until numOfAttributes).map { name =>
      new NumericAttribute(name.toString)
    }
    val attributeDataset = new AttributeDataset("matches", attributes.toArray, new NominalAttribute("doesMatch"))
    metrics.foreach { m =>
      val doubles = m.metrics.toLdLenSimDouble
      assert(numOfAttributes ==== doubles.length, s"$numOfAttributes != ${doubles.length}")
      attributeDataset.add(new attributeDataset.Row(doubles, if (m.matching) 1.0 else 0.0))
    }
    attributeDataset
  }

  private def generateClassifier(nestedTraining: List[IndexedSeq[MetricsWithResults]], nestedTest : List[IndexedSeq[MetricsWithResults]]) = {
    val training = nestedTraining.flatten
    val test = nestedTest.flatten
    logger.info(s"Training size: ${training.size}")
    logger.info(s"Test size: ${test.size}")
    logger.info(s"1s: ${training.count(_.matching)}")
    logger.info(s"0s: ${training.count(_.matching ==== false)}")
    val trainingSet = toAttributeDataSet(training)
    val scaler = new Scaler(true)
    scaler.learn(trainingSet.attributes(), trainingSet.x())
    val transformedTrainingSet = scaler.transform(trainingSet.x())
    val classifier = classification.logit(transformedTrainingSet, trainingSet.labels(), maxIter = 5000)
    val testSet = toAttributeDataSet(test)
    val testX = scaler.transform(testSet.x())
    val testY = testSet.labels()
    val pred = testX.map(classifier.predict)

    logger.info("accuracy: " + accuracy(testY, pred).toString)
    logger.info("recall: " + recall(testY, pred).toString)
    logger.info("sensitivity: " + sensitivity(testY, pred).toString)
    logger.info("specificity: " + specificity(testY, pred).toString)
    logger.info("fallout: " + fallout(testY, pred).toString)
    logger.info("fdr: " + fdr(testY, pred).toString)
    logger.info("f1: " + f1(testY, pred).toString)
    (classifier, scaler)
  }

  private def displayTestCase(testCase: TestCase, classifier: LogisticRegression, scaler: Scaler) = {
    val calculatedAlignment = new Aligner(classifier, scaler).align(testCase.left, testCase.right)
    val expected = new MainWindow()
    expected.setTitle("Excpected")
    expected.setContent(testCase.left, testCase.right, testCase.wordAlignment.toUnambigous)
    val actual = new MainWindow()
    actual.setTitle("Actual")
    actual.setContent(testCase.left, testCase.right, calculatedAlignment)
  }

  final class TrainLR extends Application {
    def start(stage: Stage): Unit = {
      val metrics = readDataSetAndMeasureMetrics()
      val (nestedTraining, nestedTest) = metrics.splitAt(metrics.size / 2)
      val (classifier, scaler) = generateClassifier(nestedTraining = nestedTraining.map(_._2), nestedTest = nestedTest.map(_._2))

      val f1s = nestedTest.map { case (path, singleTest) =>
        val singleDataSet = toAttributeDataSet(singleTest)
        val singleTestX = scaler.transform(singleDataSet.x())
        val singleTestY = singleDataSet.labels()
        val singlePred = singleTestX.map(classifier.predict)
        val f1Score = f1(singleTestY, singlePred)
        path -> f1Score
      }.sortBy(_._2)

      logger.info(f1s.mkString("\n"))
      write.xstream(classifier, "linear_regression.model")
      write.xstream(scaler, "linear_regression.scaler")
      f1s.headOption.foreach { case (path, _) =>
        logger.info(s"Displaying $path")
        val testCase = readTestCase(path)
        displayTestCase(testCase, classifier, scaler)
      }
    }
  }

  @SuppressWarnings(Array(Warts.AsInstanceOf))
  final class ShowOne extends Application {
    def start(stage: Stage): Unit = {

      val classifier = read.xstream("linear_regression.model").asInstanceOf[LogisticRegression]
      val scaler = read.xstream("linear_regression.scaler").asInstanceOf[Scaler]
      val testCase = readTestCase(Paths.get("//home/szkster/IdeaProjects/SeDiTo/common/target/scala-2.12/test-classes/algorithm_tests/full_tests/textblocklinked1to1_cpp"))
      displayTestCase(testCase, classifier, scaler)
    }
  }
}


final class PlotData extends FreeSpecLike {
  "plot data" ignore {
    val metrics = readDataSetAndMeasureMetrics()
    plot.plot(toAttributeDataSet(metrics.flatMap(_._2).toSet.take(10000)), '.', Array(Color.RED, Color.BLUE)).setVisible(true)
    Thread.sleep(10000*10000)
  }


  "train logistic regression" in {
    Application.launch(classOf[TrainLR])
  }

  "show difference" in {
    Application.launch(classOf[ShowOne])
  }
}
