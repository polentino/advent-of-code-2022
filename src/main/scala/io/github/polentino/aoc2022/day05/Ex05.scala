package io.github.polentino.aoc2022.day05

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Ex05 {
  private val charRx = "([A-Z])".r
  private val moveRx = "move (\\d+) from (\\d+) to (\\d+)".r

  def solveSingle(lines: List[String]): String   = solve(lines, _.reverse)
  def solveMultiple(lines: List[String]): String = solve(lines)

  private def solve(lines: List[String], orderingFn: mutable.Seq[Char] => mutable.Seq[Char] = identity) = {
    // split the whole lines into the part where the initial state is defined, and the one where the moves are
    val stateLines = lines.takeWhile(_.nonEmpty)
    val movesLines = lines.dropWhile(_.nonEmpty).tail

    // find out how many stacks are there, build a range out of it (to know where to look for crates),
    // and finally retrieve the lines where the stacks are defined
    val stackNr        = Integer.parseInt(stateLines.last.split("\\s+").last)
    val stackPositions = Range.inclusive(1, (stackNr - 1) * 4 + 1, 4).toList
    val stacks         = stateLines.dropRight(1)

    // build the initial state: create as many empty list as the number of stacks
    val emptyStackState: mutable.Seq[mutable.Seq[Char]] =
      mutable.ListBuffer.fill(stackNr)(mutable.ListBuffer.empty[Char])
    // build the stack state by filling the corresponding list, if there is a valid A-Z character
    val stackState                                      = stacks.foldLeft(emptyStackState) { case (state, line) =>
      stackPositions.zipWithIndex.foreach { (index, stackIndex) =>
        line(index) match {
          case charRx(value) => state(stackIndex) = state(stackIndex) :+ value
          case _             => state
        }
      }
      state
    }

    // parse the moves and apply them to the stack state
    movesLines.foreach {
      case moveRx(qty, f, t) =>
        val quantity             = Integer.parseInt(qty)
        val from                 = Integer.parseInt(f) - 1
        val to                   = Integer.parseInt(t) - 1
        val (removed, remaining) = stackState(from).splitAt(quantity)
        stackState(from) = remaining
        stackState(to) = orderingFn(removed) ++ stackState(to)
    }

    // combine the result
    stackState.map(_.head).mkString
  }
}
