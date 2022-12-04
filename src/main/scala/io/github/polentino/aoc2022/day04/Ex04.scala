package io.github.polentino.aoc2022.day04

object Ex04 {
  private val pattern = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r

  def solveFullOverlap(lines: List[String]) = solve(lines, containsRange)

  def solvePartialOverlap(lines: List[String]) = solve(lines, overlapsRange)

  private def solve(lines: List[String], rangeFn: (Range, Range) => Boolean) =
    lines.foldLeft(0) { case (acc, line) =>
      line match {
        case pattern(firstStart, firstEnd, secondStart, secondEnd) =>
          val firstRange  = Integer.parseInt(firstStart) to Integer.parseInt(firstEnd)
          val secondRange = Integer.parseInt(secondStart) to Integer.parseInt(secondEnd)

          if (rangeFn(firstRange, secondRange)) acc + 1
          else acc
      }
    }

  private def containsRange(firstRange: Range, secondRange: Range): Boolean =
    (firstRange.start >= secondRange.start &&
      firstRange.end <= secondRange.end) ||
      (secondRange.start >= firstRange.start &&
        secondRange.end <= firstRange.end)

  private def overlapsRange(firstRange: Range, secondRange: Range): Boolean =
    (firstRange intersect secondRange).nonEmpty

}
