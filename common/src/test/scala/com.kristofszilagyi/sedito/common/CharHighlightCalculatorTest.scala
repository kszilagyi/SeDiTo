package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.TraversableOps.RichTraversable
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.Warts.discard
import org.scalatest.Matchers._
final class CharHighlightCalculatorTest extends FreeSpecLike {

  private sealed case class Word(id: Int, s: String, desiredEdit: EditType)
  private sealed case class Line(id: Int, words: Word*) {
    def prodLine: String = words.map(_.s).mkString(" ")
  }

  private def test(leftLines: List[Line], rightLines: List[Line]) = {
    def toProdLines(testLines: List[Line]): Lines = {
      Lines(testLines.map { line =>
        line.prodLine
      }.toIndexedSeq)
    }

    def charIdxes(line: String, word: Word): (CharIdxInLine, CharIdxInLine) = {
      val start = line.indexOf(word.s)
      discard(assert(start ==== line.lastIndexOf(word.s), s"not unique: $word"))
      (CharIdxInLine(start), CharIdxInLine(start + word.s.length))
    }

    val indexedLeftLines = leftLines.zipWithIndex.map{case (s, idx) => (s, LineIdx(idx))}
    val indexedRightLines = rightLines.zipWithIndex.map{case (s, idx) => (s, LineIdx(idx))}

    val prodLeftLines = toProdLines(leftLines)
    val prodRightLines = toProdLines(rightLines)
    val lineIds = leftLines.map(_.id)
    val lineAlignment = lineIds.flatMap { lineId =>
      val left = indexedLeftLines.filter(_._1.id ==== lineId).single.getOrElse(fail(s"Bug in test: should be one, id = $lineId"))
      indexedRightLines.filter(_._1.id ==== lineId) match {
        case Nil =>
          None.toList
        case right :: Nil =>
          Some(LineMatch(left._2, right._2)).toList
        case other => fail(s"Bug in test: $other")
      }
    }
    val wordMatches = indexedLeftLines.flatMap{ case (leftLine, leftLineIdx) =>
      leftLine.words.flatMap { leftWord =>
        val allMatchingRightWords = indexedRightLines.flatMap { case (rightLine, rightLineIdx) =>
          rightLine.words.flatMap { rightWord =>
            if (leftWord.id ==== rightWord.id) Some((rightLineIdx, rightWord)).toList
            else None.toList
          }
        }

        val wordMatch = allMatchingRightWords match {
          case Nil =>
            None
          case (rightLineIdx, rightWord) :: Nil =>
            val leftLineText = prodLeftLines.get(leftLineIdx).getOrElse(s"Bug in test: $leftLineIdx")
            val rightLineText = prodRightLines.get(rightLineIdx).getOrElse(s"Bug in test: $rightLineIdx")

            val leftLineCharIdxes = charIdxes(leftLineText, leftWord)
            val rightLineCharIdxes = charIdxes(rightLineText, rightWord)
            val leftSelection = Selection.create(leftLineText, leftLineIdx, leftLineCharIdxes._1, leftLineCharIdxes._2).getAssert("")
            val rightSelection = Selection.create(rightLineText, rightLineIdx, rightLineCharIdxes._1, rightLineCharIdxes._2).getAssert("")

            Some(WordMatch(leftSelection, rightSelection))
          case other => fail(s"Bug in test, multiple matches for word: $other")
        }
        wordMatch.toList
      }
    }


    def toHighlight(lines: List[(Line, LineIdx)]): Map[LineIdx, Traversable[CharEdit]] = {
      lines.map { case (line, idx) =>
        val prodLine = line.prodLine
        val charEdits = line.words.map { word =>
          val (from, to) = charIdxes(prodLine, word)
          CharEdit(from, to, word.desiredEdit)
        }
        idx -> charEdits.toSet //only for testing equality
      }.toMap
    }

    println(s"prodLeftLines: $prodLeftLines\nprodRightLines: $prodRightLines\nwords: $wordMatches\nlines: $lineAlignment\n")
    CharHighlightCalculator.calc(prodLeftLines, prodRightLines, WordAlignment(wordMatches.toSet), LineAlignment(lineAlignment.toSet)) shouldBe
        CharHighlight(toHighlight(indexedLeftLines), toHighlight(indexedRightLines))

  }

  def selection(s: String, lineIdx: LineIdx, from: Int, to: Int): Selection = {
    Selection.create(s, lineIdx, CharIdxInLine(from), CharIdxInLine(to)).getAssert("wrong test data")
  }

  "empty" in {
    test(List.empty, List.empty)
  }

  "all the same - 1 words" in {
    val left = List(
      Line(1, Word(1, "one", Same))
    )
    val right = List(
      Line(1, Word(1, "one", Same))
    )
    test(left, right)
  }

  "all same - 2 words" in {
    val left = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Same))
    )
    val right = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Same))
    )
    test(left, right)
  }

  "all same - 3 words" in {
    val left = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Same), Word(3, "three", Same))
    )
    val right = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Same), Word(3, "three", Same))
    )
    test(left, right)
  }

  "1 word inserted" in {
    val left = List(
      Line(1, Word(1, "one", Same), Word(3, "three", Same))
    )
    val right = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Inserted), Word(3, "three", Same))
    )
    test(left, right)
  }

  "1 word removed" in {
    val left = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Deleted), Word(3, "three", Same))
    )
    val right = List(
      Line(1, Word(1, "one", Same), Word(3, "three", Same))
    )
    test(left, right)
  }

  //todo test word with change (test framework is not robust enough)
  "word moved from other line" in {
    val left = List(
      Line(1, Word(1, "one", Same)),
      Line(2, Word(2, "two", CharsMoved(selection("two one", LineIdx(0), 0, 3))))
    )
    val right = List(
      Line(1, Word(2, "two", CharsMoved(selection("two", LineIdx(1), 0, 3))), Word(1, "one", Same))
    )
    test(left, right)
  }


  "word moved to other line" in {
    val left = List(
      Line(1, Word(2, "two", CharsMoved(selection("two", LineIdx(1), 0, 3))), Word(1, "one", Same))
    )
    val right = List(
      Line(1, Word(1, "one", Same)),
      Line(2, Word(2, "two", CharsMoved(selection("two one", LineIdx(0), 0, 3))))
    )

    test(left, right)
  }

  "word moved within the line" in {
    val left = List(
      Line(1, Word(1, "one", Same), Word(2, "two", Same), Word(3, "thr", CharsMoved(selection("thr one two", LineIdx(0), 0, 4))))
    )
    val right = List(
      Line(1, Word(3, "thr", CharsMoved(selection("one two thr", LineIdx(0), 8, 11))), Word(1, "one", Same), Word(2, "two", Same))
    )
    test(left, right)
  }
}
