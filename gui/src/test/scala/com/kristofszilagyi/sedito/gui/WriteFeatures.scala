package com.kristofszilagyi.sedito.gui

import java.nio.file.{Files, Paths}

import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureFeatures
import com.kristofszilagyi.sedito.gui.TrainPass2.calcFirstPass
import smile.feature.Scaler
import smile.write

import scala.collection.JavaConverters._

object Write {
  def writeSamplesCSV(baseFileName: String, scaler: Scaler, data: List[Pass1PathAndSamples]): Unit = {
    val metrics = data.flatMap(_.samples.featuresWithResults.map(_.features))
    val labels = data.flatMap(_.samples.featuresWithResults.map(_.matching))
    write.csv(scaler.transform(metrics.map(_.doubles).toArray), s"$baseFileName-features.csv")
    discard(Files.write(Paths.get(s"$baseFileName-labels.csv"), labels.map(b => if (b) "1" else "0").asJava))
  }

  def writeResults(baseFileName: String, resultsByPath: List[TrainPass2.PathAndPass1Results]): Unit = {
    val results = resultsByPath.flatMap(_.pass1Results)
    val predicted = results.map(_.pass1Result.probability.toString)
    val expected = results.map(res => if (res.shouldBeMatching) "1" else "0")
    discard(Files.write(Paths.get(s"$baseFileName-predicted.csv"), predicted.asJava))
    discard(Files.write(Paths.get(s"$baseFileName-expected.csv"), expected.asJava))
  }
}

object WriteFeatures {
  def main(args: Array[String]): Unit = {
    val samples = readDataSetAndMeasureFeatures()

    val (_, firstScaler) = Main.loadAI()
    val (trainingSamples, testSamples) = Train1Pass.shuffle(samples).splitAt((samples.size * Train.trainingRatio).toInt)
    Write.writeSamplesCSV("training", firstScaler, trainingSamples)
    Write.writeSamplesCSV("test", firstScaler, testSamples)
  }
}

object WritePass1Results {
  def main(args: Array[String]): Unit = {
    val samples = readDataSetAndMeasureFeatures()
    val firstPassResultsWithPath = calcFirstPass(samples)
    val (training, test) = Train1Pass.shuffle(firstPassResultsWithPath).splitAt((samples.size * Train.trainingRatio).toInt)
    Write.writeResults("training", training)
    Write.writeResults("test", test)
  }
}
