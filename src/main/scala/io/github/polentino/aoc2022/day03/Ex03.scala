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
      .computePriorities

  // https://en.wikipedia.org/wiki/ASCII#Printable_characters
  private def assignScore(char: Char) = char.toInt - (if (char.isUpper) 38 else 96)

  private case class Accumulator(pastGroups: List[List[String]] = Nil, currentGroup: List[String] = Nil) {

    def store(value: String): Accumulator = {
      if (currentGroup.length == 3) copy(pastGroups = pastGroups :+ currentGroup, currentGroup = List(value))
      else copy(currentGroup = currentGroup :+ value)
    }

    def computePriorities: Int = (pastGroups :+ currentGroup)
      .map { group =>
        val firstLine  = group(0).toCharArray
        val secondLine = group(1).toCharArray
        val thirdLine  = group(2).toCharArray
        (firstLine intersect secondLine intersect thirdLine)
          .distinct
          .map(assignScore)
          .sum
      }.sum
  }
}
