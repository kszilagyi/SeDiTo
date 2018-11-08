package com.kristofszilagyi.sedito.gui

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.google.common.jimfs.Jimfs
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common.{FullText, UnambiguousWordAlignment}
import javafx.application.{Application, Platform}
import javafx.stage.Stage
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

import scala.collection.JavaConverters._


final class TestApp extends Application() {
  override def start(stage: Stage): Unit = {
    val lineEnding = getParameters.getRaw.asScala.head
    val diffPane = new DiffPane()
    val leftText = FullText(s"first left line${lineEnding}second left line")
    val rightText = FullText(s"first right line${lineEnding}second right line")
    diffPane.openTestCase(leftText, rightText, UnambiguousWordAlignment(Set.empty), showing = false)
    val jimfs = Jimfs.newFileSystem()
    val leftPath = jimfs.getPath("/left")
    val rightPath = jimfs.getPath("/right")
    discard(diffPane.saveFiles(leftPath, rightPath) shouldBe Saved)
    val leftContent = Files.readAllBytes(leftPath)
    val rightContent = Files.readAllBytes(rightPath)
    discard(leftContent shouldBe leftText.s.getBytes(StandardCharsets.UTF_8))
    discard(rightContent shouldBe rightText.s.getBytes(StandardCharsets.UTF_8))
    Platform.exit()
  }
}


final class TestLineEndingPreservationLinux extends FreeSpecLike {

  "Linux line endings should preserve" in {
    Application.launch(classOf[TestApp], "\n")
  }
}

final class TestLineEndingPreservationWindows extends FreeSpecLike {

  "Windows line endings should preserve" in {
    Application.launch(classOf[TestApp], "\r\n")
  }
}
