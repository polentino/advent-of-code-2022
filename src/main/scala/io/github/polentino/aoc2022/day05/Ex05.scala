package io.github.polentino.aoc2022.day05

import scala.collection.mutable

object Ex05 {
  private val charRx = "([A-Z])".r
  private val moveRx = "move (\\d+) from (\\d+) to (\\d+)".r

  def solveSingle(lines: List[String]): String   = solve(lines, _.reverse)
  def solveMultiple(lines: List[String]): String = solve(lines)

  private def solve(lines: List[String], orderingFn: Seq[Char] => Seq[Char] = identity) = {
    // split the whole lines into the part where the initial state is defined, and the one where the moves are
    val stateLines = lines.takeWhile(_.nonEmpty)
    val movesLines = lines.dropWhile(_.nonEmpty).tail

    // find out how many stacks are there, build a range out of it (to know where to look for crates),
    // and finally retrieve the lines where the stacks are defined
    val stackNr        = Integer.parseInt(stateLines.last.split("\\s+").last)
    val stackPositions = Range.inclusive(1, (stackNr - 1) * 4 + 1, 4).toList
    val stacks         = stateLines.dropRight(1)

    // build the stack state by filling the corresponding list, if there is a valid A-Z character
    val initialStackState = stacks.foldLeft(StackState(stackNr)) { case (state, line) =>
      stackPositions.zipWithIndex.foldLeft(state) { case (state, (index, stackIndex)) =>
        line(index) match {
          case charRx(value) => state.addElement(value, stackIndex)
          case _             => state
        }
      }
    }

    // parse the moves and apply them to the stack state
    val finalStackState = movesLines.foldLeft(initialStackState) { case (state, line) =>
      line match {
        case moveRx(qty, f, t) =>
          val quantity = Integer.parseInt(qty)
          val from     = Integer.parseInt(f) - 1
          val to       = Integer.parseInt(t) - 1
          state.move(quantity, from, to, orderingFn)
      }
    }

    finalStackState.result
  }

  private final case class StackState(state: Seq[Seq[Char]] = Nil) {

    def addElement(ch: Char, position: Int) =
      copy(state = state.updated(position, state(position) :+ ch))

    def move(quantity: Int, from: Int, to: Int, orderingFn: Seq[Char] => Seq[Char]) = {
      val (removed, remaining) = state(from).splitAt(quantity)
      val updated              = state
        .updated(from, remaining)
        .updated(to, orderingFn(removed) ++ state(to))
      copy(state = updated)
    }

    def result = state.map(_.head).mkString
  }

  private object StackState {
    def apply(size: Int): StackState = StackState(Seq.fill(size)(Seq.empty[Char]))
  }

}
