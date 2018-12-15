package com.kristofszilagyi.sedito.gui

import java.util.function.IntFunction

import com.kristofszilagyi.sedito.common.Warts
import javafx.scene.Node
import org.fxmisc.richtext.{GenericStyledArea, LineNumberFactory}

/**
  * This wraps a LineNumberFactory and remembers the elements. The point of this is that I can add css styles dynamically to
  * those nodes
  */
final class CachedLineNumberFactory(area: GenericStyledArea[_, _, _]) extends IntFunction[Node]{
  private val factory = LineNumberFactory.get(area)
  @SuppressWarnings(Array(Warts.Var))
  private var cache: Map[Int, Node] = Map.empty

  def apply(line: Int): Node = {
    cache.get(line) match {
      case Some(node) => node
      case None =>
        val newNode = factory.apply(line)
        cache += (line -> newNode)
        newNode
    }
  }

  def reset(): Unit = {
    cache = Map.empty
  }
}