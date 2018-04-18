package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.TraversableOps.RichTraversable
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.Warts.discard
import org.scalatest.Matchers._
final class CharHighlightCalculatorTest extends FreeSpecLike {

  trait Section {
    def s: String
  }
  private object Space extends Section {
    def s: String = " "
  }
  @SuppressWarnings(Array(Warts.Overloading))
  private object Word {
    def apply(id: Int, s: String, desiredEdit: CharEditType): Word = new Word(id, Seq(WordPart(s, desiredEdit)))
  }
  private sealed case class Word(id: Int, parts: Seq[WordPart]) extends Section {
    def s: String = parts.map(_.s).mkString
  }
  private sealed case class WordPart(s: String, desiredEdit: CharEditType)
  private sealed case class Line(id: Int, sections: Section*) {
    def prodLine: String = sections.map(_.s).mkString
    def words: Seq[Word] = sections.collect{case w: Word => w}
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

    def partCharIdxes(line: String, parts: Traversable[WordPart]): Traversable[(CharIdxInLine, CharIdxInLine, CharEditType)] = {
      parts.map { p =>
        val start = line.indexOf(p.s)
        discard(assert(start ==== line.lastIndexOf(p.s), s"not unique: $p"))
        (CharIdxInLine(start), CharIdxInLine(start + p.s.length), p.desiredEdit)
      }
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
        val charEdits = line.words.flatMap { word =>
          val ranges = partCharIdxes(prodLine, word.parts)
          ranges.map { case (from, to, desiredEdit) =>
            CharEdit(from, to, desiredEdit)
          }
        }
        idx -> charEdits.toSet //only for testing equality
      }.toMap
    }

    println(s"prodLeftLines: $prodLeftLines\nprodRightLines: $prodRightLines\nwords: $wordMatches\nlines: $lineAlignment\n")
    toSortedHightligh(CharHighlightCalculator.calc(prodLeftLines, prodRightLines, UnambiguousWordAlignment(wordMatches.toSet),
      UnambiguousLineAlignment(lineAlignment.toSet))
    ) shouldBe
      toSortedHightligh(CharHighlight(toHighlight(indexedLeftLines), toHighlight(indexedRightLines)))

  }

  private def sortedSide(side: Map[LineIdx, scala.Traversable[CharEdit]]) = {
    side.toList.map{ case (idx, edits) =>
      idx -> edits.toList.sortBy(_.from.i)
    }.sortBy{ case (lineIdx, edits) => (lineIdx.i, edits.headOption.map(_.from.i))}
  }
  private def toSortedHightligh(hl: CharHighlight) = {
    (sortedSide(hl.left), sortedSide(hl.right))
  }


  def selection(s: String, lineIdx: LineIdx, from: Int, to: Int): Selection = {
    Selection.create(s, lineIdx, CharIdxInLine(from), CharIdxInLine(to)).getAssert("wrong test data")
  }

  "empty" in {
    test(List.empty, List.empty)
  }

  "all the same - 1 words" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame))
    )
    test(left, right)
  }

  "all same - 2 words" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame))
    )
    test(left, right)
  }

  "all same - 3 words" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame), Space, Word(3, "three", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame), Space, Word(3, "three", CharsSame))
    )
    test(left, right)
  }

  "1 word inserted" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(3, "three", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsInserted), Space, Word(3, "three", CharsSame))
    )
    test(left, right)
  }

  "1 word removed" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsDeleted), Space, Word(3, "three", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(3, "three", CharsSame))
    )
    test(left, right)
  }

  "1 word matching but changed (add)" in {
    val left = List(
      Line(1, Word(1, "orange", CharsSame))
    )
    val right = List(
      Line(1, Word(1, Seq(WordPart("orange", CharsSame), WordPart("juice", CharsInserted))))
    )
    test(left, right)
  }

  "1 word matching but changed (removed)" in {
    val left = List(
      Line(1, Word(1, Seq(WordPart("orange", CharsSame), WordPart("juice", CharsDeleted))))
    )
    val right = List(
      Line(1, Word(1, "orange", CharsSame))
    )
    test(left, right)
  }

  "1 word matching but changed (add & removed)" in {
    val left = List(
      Line(1, Word(1, Seq(WordPart("orange", CharsSame), WordPart("juice", CharsDeleted))))
    )
    val right = List(
      Line(1, Word(1,  Seq(WordPart("orange", CharsSame), WordPart("moot", CharsInserted))))
    )
    test(left, right)
  }

  "word moved from other line" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame)),
      Line(2, Word(2, "two", CharsMoved(selection("two one", LineIdx(0), 0, 3), Traversable.empty)))
    )
    val right = List(
      Line(1, Word(2, "two", CharsMoved(selection("two", LineIdx(1), 0, 3), Traversable.empty)), Space, Word(1, "one", CharsSame))
    )
    test(left, right)
  }


  "word moved to other line" in {
    val left = List(
      Line(1, Word(2, "two", CharsMoved(selection("two", LineIdx(1), 0, 3), Traversable.empty)), Space, Word(1, "one", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame)),
      Line(2, Word(2, "two", CharsMoved(selection("two one", LineIdx(0), 0, 3), Traversable.empty)))
    )

    test(left, right)
  }

  "word moved from other line but changed" in {
    val left = List(
      Line(1, Word(1, "one", CharsSame)),
      Line(2, Word(2, "two", CharsMoved(selection("two one", LineIdx(0), 0, 3), Traversable.empty)))
    )
    val right = List(
      Line(1, Word(2, "two", CharsMoved(selection("two", LineIdx(1), 0, 3), Traversable.empty)), Space, Word(1, "one", CharsSame))
    )
    test(left, right)
  }


  "word moved to other line but changed" in {
    val left = List(
      Line(1, Word(2, "two", CharsMoved(selection("two", LineIdx(1), 0, 3), Traversable.empty)), Space, Word(1, "one", CharsSame))
    )
    val right = List(
      Line(1, Word(1, "one", CharsSame)),
      Line(2, Word(2, "two", CharsMoved(selection("two one", LineIdx(0), 0, 3), Traversable.empty)))
    )

    test(left, right)
  }

  "word moved within the line" in {
    def move(line: String, lineIdx: LineIdx, from: Int, to: Int) = {
      CharsMoved(selection(line, lineIdx, from, to), Traversable.empty)
    }
    val left = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame), Space, Word(3, "thr", move("thr one two", LineIdx(0), 0, 3)))
    )
    val right = List(
      Line(1, Word(3, "thr", move("one two thr", LineIdx(0), 8, 11)), Space, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame))
    )
    test(left, right)
  }

  "word moved and changed within line" in  {
    def move(line: String, lineIdx: LineIdx, from: Int, to: Int, editPosition: Int, insert: Boolean) = {
      val tpe = if (insert) CharsInserted else CharsDeleted
      CharsMoved(selection(line, lineIdx, from, to + 1), Traversable(
        ActualCharEdit(CharIdxInLine(editPosition), CharIdxInLine(editPosition + 1), tpe)
      ))
    }
    val left = List(
      Line(1, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame), Space,
        Word(3, "thra", move("thrb one two", LineIdx(0), 0, 3, editPosition = 11, insert = false)))
    )
    val right = List(
      Line(1, Word(3, "thrb", move("one two thra", LineIdx(0), 8, 11, editPosition = 3, insert = true)),
        Space, Word(1, "one", CharsSame), Space, Word(2, "two", CharsSame))
    )
    test(left, right)
  }
}
