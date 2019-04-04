package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.gui.Train.{countFN, countFP, countTP}
import org.log4s.getLogger
import smile.validation._
import TrainingData.logger
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

final class YAndPred(val y: Array[Int],val pred: Array[Int])

final case class BasicMetrics(trainingF1: Double, testF1: Double, training0s: Int, training1s: Int, test0s: Int, test1s: Int) {
  def all: String = {
    val trainingSize = training1s + training0s
    val training1Percentage = training1s.toDouble / trainingSize.toDouble

    val testSize = test1s + test0s
    val test1Percentage = test1s.toDouble / testSize.toDouble
    s"Training: $trainingF1, test: $testF1, training size: $trainingSize ($training1Percentage positive)," +
      s" test size: $testSize ($test1Percentage positive),"
  }

  def f1s: String = s"Training: $trainingF1, test: $testF1"

  def +(other: BasicMetrics): BasicMetrics = {
    BasicMetrics(
      trainingF1 = trainingF1 + other.trainingF1,
      testF1 = testF1 + other.testF1,
      training0s = training0s + other.training0s,
      training1s = training1s + other.training1s,
      test0s = test0s + other.test0s,
      test1s = test1s + other.test1s
    )
  }

  def /(i: Int): BasicMetrics = {
    BasicMetrics(trainingF1 = trainingF1 / i, testF1 = testF1 / i,
      training0s = training0s / i, training1s = training1s / i,
      test0s = test0s / i, test1s = test1s / i)
  }
}
object TrainingData {
  private val logger = getLogger

}
final case class TrainingData(training: YAndPred, test: YAndPred) {
  def f1s: BasicMetrics = {
    val trainingF1 = f1(training.y, training.pred)
    val testF1 = f1(test.y, test.pred)
    BasicMetrics(trainingF1 = trainingF1, testF1 = testF1,
      training0s = training.y.count(_ ==== 0), training1s = training.y.count(_ ==== 1),
      test0s = test.y.count(_ ==== 0), test1s = test.y.count(_ ==== 1))
  }

  def printDetailedStats(f1s: BasicMetrics): Unit = {
    val trainingY = training.y
    val trainingPred = training.pred
    val testY = test.y
    val testPred = test.pred
    logger.info("training accuracy: " + accuracy(trainingY, trainingPred).toString)
    logger.info("training recall: " + recall(trainingY, trainingPred).toString)
    logger.info("training sensitivity(TP / (TP + FN)): " + sensitivity(trainingY, trainingPred).toString)
    logger.info("training specificity(TN / (FP + TN)): " + specificity(trainingY, trainingPred).toString)
    logger.info("training fallout(FP / (FP + TN)): " + fallout(trainingY, trainingPred).toString)
    logger.info("training fdr(FP / (TP + FP)): " + fdr(trainingY, trainingPred).toString)
    val trainFp = countFP(trainingY, trainingPred)
    val trainTp = countTP(trainingY, trainingPred)
    val trainFn = countFN(trainingY, trainingPred)
    logger.info(s"training TP count: $trainTp")
    logger.info(s"training FP count: $trainFp")
    logger.info(s"training FN count: $trainFn")
    logger.info(s"total mispred: ${trainFp + trainFn}")
    logger.info(s"training f1: ${f1s.trainingF1}")

    logger.info("test accuracy: " + accuracy(testY, testPred).toString)
    logger.info("test recall: " + recall(testY, testPred).toString)
    logger.info("test sensitivity(TP / (TP + FN)): " + sensitivity(testY, testPred).toString)
    logger.info("test specificity(TN / (FP + TN)): " + specificity(testY, testPred).toString)
    logger.info("test fallout(FP / (FP + TN)): " + fallout(testY, testPred).toString)
    logger.info("test fdr(FP / (TP + FP)): " + fdr(testY, testPred).toString)
    val testFp = countFP(testY, testPred)
    val testTp = countTP(testY, testPred)
    val testFn = countFN(testY, testPred)
    logger.info(s"test TP count: $testTp")
    logger.info(s"test FP count: $testFp")
    logger.info(s"test FN count: $testFn")
    logger.info(s"total mispred: ${testFp + testFn}")
    logger.info(s"test f1: ${f1s.testF1}")
  }
}
