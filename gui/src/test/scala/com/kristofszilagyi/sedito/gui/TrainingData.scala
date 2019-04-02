package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.gui.Train.{countFN, countFP, countTP}
import org.log4s.getLogger
import smile.validation._
import TrainingData.logger

final class YAndPred(val y: Array[Int],val pred: Array[Int])
final case class F1s(training: Double, test: Double) {
  override def toString: String = {
    s"Training: $training, test: $test"
  }

  def +(f1s: F1s): F1s = {
    F1s(training = training + f1s.training, test = test + f1s.test)
  }

  def /(i: Int): F1s = {
    F1s(training = training / i, test = test / i)
  }
}
object TrainingData {
  private val logger = getLogger

}
final case class TrainingData(training: YAndPred, test: YAndPred) {
  def f1s: F1s = {
    val trainingF1 = f1(training.y, training.pred)
    val testF1 = f1(test.y, test.pred)
    F1s(training = trainingF1, test = testF1)
  }

  def printDetailedStats(f1s: F1s): Unit = {
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
    logger.info(s"training f1: ${f1s.training}")

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
    logger.info(s"test f1: ${f1s.test}")
  }
}
