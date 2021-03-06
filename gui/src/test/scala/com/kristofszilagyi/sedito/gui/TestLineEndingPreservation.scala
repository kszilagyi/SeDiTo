package com.kristofszilagyi.sedito.gui

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.google.common.jimfs.Jimfs
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common.{FullText, UnambiguousWordAlignment}
import javafx.application.{Application, Platform}
import javafx.stage.Stage
import org.scalatest.{FreeSpecLike, Tag}
import org.scalatest.Matchers._

import scala.collection.JavaConverters._

//travis can't create gui stuff
object NoCI extends Tag("com.kristofszilagyi.sedito.gui.NoCI")

final class TestApp extends Application() {
  override def start(stage: Stage): Unit = {
    val lineEnding = getParameters.getRaw.asScala.head
    val diffPane = new DiffPane()
    val leftText = FullText(s"first left line${lineEnding}second left line")
    val rightText = FullText(s"first right line${lineEnding}second right line")
    val jimfs = Jimfs.newFileSystem()
    val leftPath = jimfs.getPath("/left")
    val rightPath = jimfs.getPath("/right")
    diffPane.open(leftText, rightText, Some(leftPath), Some(rightPath), UnambiguousWordAlignment(Set.empty), showing = false)
    discard(diffPane.saveFiles() shouldBe ((Saved, Saved)))
    val leftContent = Files.readAllBytes(leftPath)
    val rightContent = Files.readAllBytes(rightPath)
    discard(leftContent shouldBe leftText.s.getBytes(StandardCharsets.UTF_8))
    discard(rightContent shouldBe rightText.s.getBytes(StandardCharsets.UTF_8))
    Platform.exit()
  }
}


final class TestLineEndingPreservationLinux extends FreeSpecLike {

  "Linux line endings should preserve" taggedAs(NoCI) in {
    Application.launch(classOf[TestApp], "\n")
  }
}

final class TestLineEndingPreservationWindows extends FreeSpecLike {

  "Windows line endings should preserve" taggedAs(NoCI)  in {
    Application.launch(classOf[TestApp], "\r\n")
  }
}

final class TestLineEndingPreservationMac extends FreeSpecLike {

  "Mac line endings should preserve" taggedAs(NoCI)  in {
    Application.launch(classOf[TestApp], "\r")
  }
}