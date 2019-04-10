package com.kristofszilagyi.sedito.gui

import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.gui.Train1Pass.{hiddenLayerSize, order}
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureFeatures
import org.log4s.getLogger
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.write
//todo probably should filter out the cases which I do not want to tackle yet and only teach with those which I want
object LearningCurve{
  private val logger = getLogger

  private def oneCurve(training: List[Pass1PathAndSamples], test: List[Pass1PathAndSamples]) = {
    val learningCurve = ((1 to (training.size, 6)).par map { size =>
      logger.info(s"Doing size: $size")
      val (_, _, basicMetrics) = Train.train(training.take(size), test, logStats = false, hiddenLayerSize = hiddenLayerSize)

      logger.info(s"Finished size: $size")
      size -> ((basicMetrics.trainingF1, basicMetrics.testF1)) //we don't measure the joint algo just the nn
    }).seq

    learningCurve
  }
  private def toCoords(data: Seq[(Int, (Double, Double))]) = {
    val trainCoords = data.map { case (size, (train, _)) =>
      Array(size.toDouble, train)
    }.toArray
    val testCoords = data.map { case (size, (_, test)) =>
      Array(size.toDouble, test)
    }.toArray

    val attrs = new AttributeDataset("coords", Array(new NumericAttribute("x"), new NumericAttribute("y")),
      new NominalAttribute("isTraining", Array("false", "true")))
    trainCoords.foreach { xs =>
      attrs.add(xs, 1)
    }

    testCoords.foreach { xs =>
      attrs.add(xs, 0)
    }
    attrs
  }
  def main(args: Array[String]): Unit = {
    logger.info("Start")
    val start = Instant.now()

    val orderedSamples = order(readDataSetAndMeasureFeatures())
    val trainingSize = (orderedSamples.size * Train.trainingRatio).toInt
    val testSize = orderedSamples.size - trainingSize
    val learningCurves = (0 to 4) map { run =>
      logger.info(s"Doing run : $run")

      val testStart = run * testSize
      val testEnd = testStart + testSize
      logger.info(s"testStart: $testStart, testEnd: $testEnd")
      val training = orderedSamples.slice(0, testStart) ++ orderedSamples.drop(testEnd)
      val test = orderedSamples.slice(testStart, testEnd)

      oneCurve(training, test)
    }

    val flattenedLearningCurves = learningCurves.flatten
    val flattenedCoords = toCoords(flattenedLearningCurves)

    write.arff(flattenedCoords, "flattened_learning_curve.arff")

    val avgLearningCurve = flattenedLearningCurves.groupBy(_._1).map { case(runs, results) =>
      val f1Sums = results.map(_._2).reduce[(Double, Double)]{ case (r1, r2) =>
        (r1._1 + r2._1, r1._2 + r2._2)
      }
      val cnt = results.size
      runs -> ((f1Sums._1 / cnt, f1Sums._2 / cnt))
    }
    val avgCoords = toCoords(avgLearningCurve.toSeq.sortBy(_._1)) //for stability
    write.arff(avgCoords, "avg_learning_curve.arff")

    val duration = Duration.between(start, Instant.now())
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis/1000 - duration.toMinutes * 60} seconds")
  }
}