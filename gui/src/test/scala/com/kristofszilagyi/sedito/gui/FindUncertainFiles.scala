package com.kristofszilagyi.sedito.gui

import java.io.{File, FileWriter}

import com.kristofszilagyi.sedito.aligner.{Pass1Aligner, Pass1FeatureCalculator}
import com.kristofszilagyi.sedito.common.FullText
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.utils.Control._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.diff.DiffEntry.ChangeType
import org.eclipse.jgit.lib.{ObjectId, ObjectReader}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.log4s.getLogger

import scala.collection.JavaConverters._
import scala.io.Source

object FindUncertainFiles {
  private val logger = getLogger

  private def idToFileString(reader: ObjectReader, objectId: ObjectId) = {
    val data = reader.open(objectId.toObjectId).getBytes
    new String(data, "utf-8")
  }
  def main(args: Array[String]): Unit = {
    val repoLocation = "."
    val builder = new FileRepositoryBuilder
    val repository = builder.setWorkTree(new File(repoLocation).getAbsoluteFile).readEnvironment.findGitDir
      .build

    val git = new Git(repository)
    val reader = repository.newObjectReader()
    val commits = git.log().all().call().iterator().asScala.toVector
    val (classifier, scaler) = Main.loadAI()
    val aligner = new Pass1Aligner(classifier, scaler)
    val file = new File("uncertain.txt")
    val commitAndFilenames = using(Source.fromFile(file)){_.getLines().map(_.split(";").head).toSet}
    val writer = new FileWriter(file, true)

    commits.foreach { commit =>
      val currentTree = new CanonicalTreeParser()
      currentTree.resetRoot(reader, commit.getTree)

      val parentTree = new CanonicalTreeParser()
      parentTree.reset(reader, commit.getParent(0).getTree)

      git.diff().setNewTree(currentTree).setOldTree(parentTree).call().asScala.foreach{ diff =>
        if (diff.getChangeType ==== ChangeType.MODIFY) {
          val commitAndFilename = s"${commit.getId.name}/${diff.getNewPath}"
          if (!commitAndFilenames.contains(commitAndFilename)) {
            logger.info(s"Doing $commitAndFilename")
            val left = FullText(idToFileString(reader, diff.getOldId.toObjectId))
            val right = FullText(idToFileString(reader, diff.getNewId.toObjectId))
            if (left.s.length < 40000 && right.s.length < 40000) {

              val metrics = Pass1FeatureCalculator.calcAlignerFeatures(left, right)
              val predictions = aligner.findPotentialMatches(metrics, minP = 0.0)
              val uncertain = predictions.count(p => p.probability > 0.01 && p.probability < 0.99)
              val quiteUncertain = predictions.count(p => p.probability > 0.1 && p.probability < 0.9)
              val veryUncertain = predictions.count(p => p.probability > 0.2 && p.probability < 0.8)
              val uncertainDensity = uncertain.toDouble / predictions.size.toDouble
              val quiteUncertainDensity = quiteUncertain.toDouble / predictions.size.toDouble
              val veryUncertainDensity = veryUncertain.toDouble / predictions.size.toDouble
              val resultString = s"$commitAndFilename;$uncertain;$quiteUncertain;$veryUncertain;$uncertainDensity;" +
                s"$quiteUncertainDensity;$veryUncertainDensity\n"
              writer.append(resultString)
              writer.flush()
            } else {
              logger.info(s"Skipping file as it's too big: $commitAndFilename")
            }
          } else {
            logger.info(s"Skipping $commitAndFilename")
          }
        }

      }
    }

  }
}
