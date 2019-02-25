package com.kristofszilagyi.sedito.gui

import java.nio.file.{Files, Paths}

import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureFeatures
import smile.feature.Scaler
import smile.write
import scala.collection.JavaConverters._

object Write {
  def writeCSV(baseFileName: String, scaler: Scaler, data: List[Pass1PathAndSamples]): Unit = {
    val metrics = data.flatMap(_.samples.featuresWithResults.map(_.features))
    val labels = data.flatMap(_.samples.featuresWithResults.map(_.matching))
    write.csv(scaler.transform(metrics.map(_.doubles).toArray), s"$baseFileName-features.csv")
    discard(Files.write(Paths.get(s"$baseFileName-labels.csv"), labels.map(b => if (b) "1" else "0").asJava))
  }
}

object WriteFeatures {
  def main(args: Array[String]) {
    val samples = readDataSetAndMeasureFeatures()

    val (_, firstScaler) = Main.loadAI()
    val (trainingSamples, testSamples) = Train1Pass.shuffle(samples).splitAt((samples.size * Train.trainingRatio).toInt)
    Write.writeCSV("training", firstScaler, trainingSamples)
    Write.writeCSV("test", firstScaler, testSamples)
  }
}

object WritePass1Results {

}
