package io.github.polentino.aoc2022.day01

object Ex01 {

  def findMax(lines: List[String]): Int         = solve(lines).head
  def findTopThreeMax(lines: List[String]): Int = solve(lines).take(3).sum

  private def solve(lines: List[String]): Seq[Int] =
    lines
      .foldLeft(Accumulator()) { case (accumulator, current) =>
        if (current.isBlank) accumulator.advance
        else accumulator.store(Integer.parseInt(current))
      }
      .sumAndSort

  private case class Accumulator(pastLines: List[List[Int]] = Nil, currentLines: List[Int] = Nil) {

    def advance: Accumulator =
      copy(pastLines = pastLines :+ currentLines, currentLines = Nil)

    def store(value: Int): Accumulator =
      copy(currentLines = currentLines :+ value)

    def sumAndSort: Seq[Int] =
      (pastLines :+ currentLines)
        .map(_.sum)
        .sorted(Ordering.Int.reverse)
  }
}
