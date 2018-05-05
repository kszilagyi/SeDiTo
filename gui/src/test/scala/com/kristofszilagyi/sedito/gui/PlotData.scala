package com.kristofszilagyi.sedito.gui

import java.awt.Color
import java.nio.file.{Files, Paths}

import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.aligner.{Aligner, MetricCalculator}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, WordMatch}
import com.kristofszilagyi.sedito.gui.PlotData.{generateClassifier, logger, readDataSetAndMeasureMetrics, toAttributeDataSet}
import org.log4s.getLogger
import org.scalatest.FreeSpecLike
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.validation._
import smile.{classification, plot}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final case class MetricsWithResults(metrics: Metrics, matching: Boolean)

object PlotData {
  private val logger = getLogger

  private def readDataSetAndMeasureMetrics() = {
    val parentDir = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    val testDirs = using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p))
    }
    val metrics = testDirs.map{ testDir =>
      TestCase.open(testDir) match {
        case Failure(exception) =>
          println(s"$testDir -> ${exception.getMessage}")
          sys.exit(1)
        case Success(testCase) =>
          val metrics = MetricCalculator.calcAlignerMetrics(testCase.left, testCase.right)

          val matches = testCase.wordAlignment.matches.toSeq
          val matchesSet = matches.toSet
          discard(assert(matches.size ==== matchesSet.size))

          testDir -> metrics.map { m =>
            val potentialMatch = WordMatch(m.leftWord, m.rightWord)
            MetricsWithResults(m, matching = matchesSet.contains(potentialMatch))
          }
      }
    }
    metrics
  }

  def toAttributeDataSet(metrics: Traversable[MetricsWithResults]) = {
    val attributes = List("ldLenSim", "ldLenSimBefore", "ldLenSimAfter").map { name =>
      new NumericAttribute(name)
    }
    val attributeDataset = new AttributeDataset("matches", attributes.toArray, new NominalAttribute("doesMatch"))
    metrics.foreach { m =>
      val doubles = m.metrics.toLdLenSimDouble
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
    val classifier = classification.logit(trainingSet.x(), trainingSet.labels())
    val testSet = toAttributeDataSet(test)
    val testX = testSet.x()
    val testY = testSet.labels()
    val pred = testX.map(classifier.predict)

    logger.info("accuracy: " + accuracy(testY, pred).toString)
    logger.info("recall: " + recall(testY, pred).toString)
    logger.info("sensitivity: " + sensitivity(testY, pred).toString)
    logger.info("specificity: " + specificity(testY, pred).toString)
    logger.info("fallout: " + fallout(testY, pred).toString)
    logger.info("fdr: " + fdr(testY, pred).toString)
    logger.info("f1: " + f1(testY, pred).toString)
    classifier
  }

}
final class PlotData extends FreeSpecLike {
  "plot data" ignore {
    val metrics = readDataSetAndMeasureMetrics()
    plot.plot(toAttributeDataSet(metrics.flatMap(_._2).toSet.take(10000)), '.', Array(Color.RED, Color.BLUE)).setVisible(true)
    Thread.sleep(10000*10000)
  }


  "train logistic regression" ignore {
    val metrics = readDataSetAndMeasureMetrics()
    val (nestedTraining, nestedTest) = metrics.splitAt(metrics.size / 2)
    val classifier = generateClassifier(nestedTraining = nestedTraining.map(_._2), nestedTest = nestedTest.map(_._2))

    val f1s = nestedTest.map { case (path, singleTest) =>
      val singleDataSet = toAttributeDataSet(singleTest)
      val singleTestX = singleDataSet.x()
      val singleTestY = singleDataSet.labels()
      val singlePred = singleTestX.map(classifier.predict)
      val f1Score = f1(singleTestY, singlePred)
      path -> f1Score
    }.sortBy(_._2)

    logger.info(f1s.toString)

    f1s.foreach { case (path, _) =>
      logger.info(s"Displaying $path")
      TestCase.open(path) match {
        case Failure(exception) =>
          println(s"failed to open: ${exception.getMessage}")
          sys.exit(1)
        case Success(testCase) =>
          val _ = new Aligner(classifier).align(testCase.left, testCase.right)

      }
    }
  }
}
