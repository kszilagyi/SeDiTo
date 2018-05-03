package com.kristofszilagyi.sedito.aligner

import java.awt.Color
import java.nio.file.{Files, Paths}

import com.kristofszilagyi.sedito.aligner.Aligner.Metrics
import com.kristofszilagyi.sedito.aligner.PlotData.{logger, readDataSet, toAttributeDataSet}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, WordMatch}
import org.log4s.getLogger
import org.scalatest.FreeSpecLike
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.{classification, plot}
import smile.validation._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final case class MetricsWithResults(metrics: Metrics, matching: Boolean)

object PlotData {
  private val logger = getLogger

  private def readDataSet() = {
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
          val metrics = Aligner.calcAlignerMetrics(testCase.left, testCase.right)

          val matches = testCase.wordAlignment.matches.toSeq
          val matchesSet = matches.toSet
          discard(assert(matches.size ==== matchesSet.size))

          metrics map { m =>
            val potentialMatch = WordMatch(m.leftWord, m.rightWord)
            (testDir, MetricsWithResults(m, matching = matchesSet.contains(potentialMatch)))
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

  def pickMostProbables(m: Map[])
}
final class PlotData extends FreeSpecLike {
  "plot data" in {
    val metrics = readDataSet()
    plot.plot(toAttributeDataSet(metrics.flatten.map(_._2).toSet.take(10000)), '.', Array(Color.RED, Color.BLUE)).setVisible(true)
    Thread.sleep(10000*10000)
  }


  "train logistic regression" in {
    val metrics = readDataSet()
    val (nestedTraining, nestedTest) = metrics.splitAt(metrics.size / 2)
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

    nestedTest.map { singleTest =>
      val singleDataSet = toAttributeDataSet(singleTest)
      val singleTestX = singleDataSet.x()
      val singleTestY = singleDataSet.labels()
      val singlePred = singleTestX.map(classifier.predict)
      val f1Score = f1(singleTestY, singlePred)
      //todo filter out conflicts (by taking the most probable one)
      val matches = singleTest.flatMap { case (_, m) =>
        val probs = new Array[Double](2)
        val prediction = classifier.predict(m.metrics.toLdLenSimDouble, probs)
        if (prediction ==== 1) {
          assert(probs(1) > 0.5)
          Some(WordMatch(m.metrics.leftWord, m.metrics.rightWord) -> probs(1))
        } else None
      }
      val leftGrouped = matches.groupBy(_._1.left).map(_._2).map(_.seq.sortBy(_._2).last)
      val rightGrouped =
    }
  }
}
