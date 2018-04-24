package com.kristofszilagyi.sedito.aligner

import java.nio.file.{Files, Paths}

import com.kristofszilagyi.sedito.aligner.Aligner.Metrics
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{TestCase, WordMatch}
import org.scalatest.FreeSpecLike
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final case class MetricsWithResults(metrics: Metrics, matching: Boolean)
final class PlotData extends FreeSpecLike {
  "plot data" in {
    val parentDir = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    val testDirs = using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p))
    }.take(20) //todo remove, only here for speed
    val metrics = testDirs.flatMap{ testDir =>
      TestCase.open(testDir) match {
        case Failure(exception) =>
          println(s"$testDir -> ${exception.getMessage}")
          sys.exit(1)
        case Success(testCase) =>
          val metrics = Aligner.calcAlignerMetrics(testCase.left, testCase.right)

          val matches = testCase.wordAlignment.matches.toSeq
          val matchesSet = matches.toSet
          discard(assert(matches.size ==== matchesSet.size))

          metrics map { metrics =>
            val potentialMatch = WordMatch(metrics.leftWord, metrics.rightWord)
            MetricsWithResults(metrics, matching = matchesSet.contains(potentialMatch))
          }
      }
    }


    val attributes = List("ld", "ldLenSim", "ldBefore", "ldLenSimBefore", "ldAfter", "ldLenSimAfter").map { name =>
      new NumericAttribute(name)
    }
    val attributeDataset = new AttributeDataset("matches", attributes.toArray, new NominalAttribute("doesMatch"))
    metrics.foreach { m =>
      val doubles = m.metrics.toDoubles
      doubles.foreach { d =>
        attributeDataset.add(new attributeDataset.Row(d, if (m.matching) 1.0 else 0.0))
      }
    }

    //plot.plot(attributeDataset, '.').setVisible(true)
  }

}
