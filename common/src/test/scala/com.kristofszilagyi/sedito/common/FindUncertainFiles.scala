package com.kristofszilagyi.sedito.common

import java.io.File

import TypeSafeEqualsOps._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.diff.DiffEntry.ChangeType
import org.eclipse.jgit.lib.{ObjectId, ObjectReader}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.CanonicalTreeParser

import scala.collection.JavaConverters._
object FindUncertainFiles {
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
    commits.foreach { commit =>
      println(commit.getId)
      val currentTree = new CanonicalTreeParser()
      currentTree.resetRoot(reader, commit.getTree)

      val parentTree = new CanonicalTreeParser()
      parentTree.reset(reader, commit.getParent(0).getTree)

      git.diff().setNewTree(currentTree).setOldTree(parentTree).call().asScala.foreach{ diff =>
        if (diff.getChangeType ==== ChangeType.MODIFY) {
          println(diff.getNewPath)
          println(idToFileString(reader, diff.getOldId.toObjectId))
          println(idToFileString(reader, diff.getNewId.toObjectId))
        }

      }
    }

  }
}
