package io.github.polentino.aoc2022.day03

object Ex03 {

  def sumOfPriorities(lines: List[String]) = {
    lines.map(line =>
      val firstCompartment  = line.take(line.length / 2).toCharArray
      val secondCompartment = line.drop(line.length / 2).toCharArray
      firstCompartment.intersect(secondCompartment)
      .distinct
      .map(assignScore)
      .sum
    ).sum
  }

  def sumOfGroupedPriorities(lines: List[String]) =
    lines.foldLeft(Accumulator()) { case (acc, string) => acc.store(string) }
      .store("")
      .sumOfPriorities

  // https://en.wikipedia.org/wiki/ASCII#Printable_characters
  private def assignScore(char: Char) = char.toInt - (if (char.isUpper) 38 else 96)

  private case class Accumulator(sumOfPriorities: Int = 0, currentLines: List[String] = Nil) {

    def store(value: String): Accumulator = {
      if (currentLines.length == 3) advance(value: String)
      else copy(currentLines = currentLines :+ value)
    }

    private def advance(value: String): Accumulator = {
      val firstLine        = currentLines(0).toCharArray
      val secondLine       = currentLines(1).toCharArray
      val thirdLine        = currentLines(2).toCharArray
      val commonCharsScore = (firstLine intersect secondLine intersect thirdLine)
        .distinct
        .map(assignScore)
        .sum

      copy(sumOfPriorities = sumOfPriorities + commonCharsScore, currentLines = List(value))
    }
  }
}
