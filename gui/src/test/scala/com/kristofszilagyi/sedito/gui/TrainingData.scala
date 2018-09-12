package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.gui.TrainAndDiff.{countFN, countFP}
import org.log4s.getLogger
import smile.validation._
import TrainingData.logger

final class YAndPred(val y: Array[Int],val pred: Array[Int])
final case class F1s(training: Double, test: Double)
object TrainingData {
  private val logger = getLogger

}
final case class TrainingData(training: YAndPred, test: YAndPred, lostPositivesInTraining: Int, lostPositivesInTest: Int) {
  def printDetailedStats(): Unit = {
    val trainingY = training.y
    val trainingPred = training.pred
    val testY = test.y
    val testPred = test.pred
    logger.info(s"test lost positives: $lostPositivesInTraining")
    logger.info("training accuracy: " + accuracy(trainingY, trainingPred).toString)
    logger.info("training recall: " + recall(trainingY, trainingPred).toString)
    logger.info("training sensitivity(TP / (TP + FN)): " + sensitivity(trainingY, trainingPred).toString)
    logger.info("training specificity(TN / (FP + TN)): " + specificity(trainingY, trainingPred).toString)
    logger.info("training fallout(FP / (FP + TN)): " + fallout(trainingY, trainingPred).toString)
    logger.info("training fdr(FP / (TP + FP)): " + fdr(trainingY, trainingPred).toString)
    logger.info(s"training FP count: ${countFP(trainingY, trainingPred)}")
    logger.info(s"training FN count: ${countFN(trainingY, trainingPred)}")
    logger.info("training f1: " + f1(trainingY, trainingPred).toString)

    logger.info(s"test lost positives: $lostPositivesInTest")
    logger.info("test accuracy: " + accuracy(testY, testPred).toString)
    logger.info("test recall: " + recall(testY, testPred).toString)
    logger.info("test sensitivity(TP / (TP + FN)): " + sensitivity(testY, testPred).toString)
    logger.info("test specificity(TN / (FP + TN)): " + specificity(testY, testPred).toString)
    logger.info("test fallout(FP / (FP + TN)): " + fallout(testY, testPred).toString)
    logger.info("test fdr(FP / (TP + FP)): " + fdr(testY, testPred).toString)
    logger.info(s"test FP count: ${countFP(testY, testPred)}")
    logger.info(s"test FN count: ${countFN(testY, testPred)}")
    logger.info("test f1: " + f1(testY, testPred).toString)
  }

  def f1s: F1s = {
    val trainingY = training.y
    val trainingPred = training.pred
    val testY = test.y
    val testPred = test.pred

    val trainingF1 = f1(trainingY, trainingPred)

    val testF1 = f1(testY, testPred)
    F1s(training = trainingF1, test = testF1)
  }
}
