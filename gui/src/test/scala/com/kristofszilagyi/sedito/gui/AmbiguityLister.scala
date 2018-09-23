package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.gui.TrainAndDiff.{readTestCase, testDirs}
import org.log4s.getLogger

object AmbiguityLister {
  private val logger = getLogger

  def main(args: Array[String]): Unit = {
    logger.info("Start")
    val testCases = testDirs.map(dir => dir -> readTestCase(dir))
    logger.info(s"ambig count = ${testCases.map{ case (path , test) =>
      path -> test.wordAlignment.rightAmbiguous
    }.filter(_._2.nonEmpty).mkString("\n")}")
    /*
        logger.info(s"ambig count = " + testCases.map{ case (_, test) =>
      test.wordAlignment.matches.size - test.wordAlignment.toUnambigous.matches.size
    }.sum)
     */
  }

}
