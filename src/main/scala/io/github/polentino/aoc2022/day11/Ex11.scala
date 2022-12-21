package io.github.polentino.aoc2022.day11

object Ex11 {

  def solveFirst(lines: List[String]) =
    solve(lines, 20, _ / 3)

  def solveSecond(lines: List[String]) =
    solve(lines, 10000, identity)

  private def solve(lines: List[String], rounds: Int, worryLevelFn: BigInt => BigInt) = {
    val monkeys = lines.foldLeft(Accumulator(worryLevelFn = worryLevelFn))((acc, line) => acc.advance(line)).getMonkeys

    val start = Round(monkeys)
    val end   = (1 to rounds).scanLeft(start)((round, _) => round.process).last
    end.activities.values.toList.sorted.reverse.take(2).product
  }

  private case class Accumulator(
    private val monkeys: List[Monkey] = Nil,
    lines: List[String] = Nil,
    worryLevelFn: BigInt => BigInt
  ) {

    def advance(line: String): Accumulator =
      if (line.isEmpty) copy(monkeys = monkeys :+ Monkey(lines, worryLevelFn), lines = Nil)
      else copy(lines = lines :+ line)

    def getMonkeys = monkeys :+ Monkey(lines, worryLevelFn)
  }

  private case class Round(monkeys: List[Monkey], activities: Map[BigInt, BigInt] = Map.empty) {

    def idsToProcess = monkeys.map(_.id)

    def process = {
      idsToProcess.foldLeft(this)((round, monkeyID) => {
        val throws = round.monkeys(monkeyID).inspect
        round.executeThrows(monkeyID, throws)
          .updateCounter(monkeyID, throws.size)
      })
    }

    def printWorry =
      monkeys.map(m => s"Monkey ${m.id}: ${m.startingItems.mkString(", ")}").mkString("\n")

    def printActivity =
      activities.map {
        case (monkeyId, activity) => s"Monkey $monkeyId inspected items $activity times."
      }.mkString("\n")

    private def executeThrows(fromMonkeyID: BigInt, throws: List[Throw]) = {
      val updated = throws.foldLeft(monkeys)((monkeys, t) => {
        // we assume the monkeys:
        //  1. all ids are existent, so i can safely call .get
        //  2. are all ordered, otherwise we would need exec an indexOf() first
        val from = monkeys.find(_.id == fromMonkeyID).get
        val to   = monkeys.find(_.id == t.to).get
        monkeys
          .updated(from.id, from.copy(startingItems = from.startingItems.filterNot(_ == t.originalWorry)))
          .updated(to.id, to.copy(startingItems = to.startingItems :+ t.item))
      })

      copy(monkeys = updated)
    }

    private def updateCounter(monkeyId: BigInt, count: BigInt): Round =
      copy(activities = activities.updated(monkeyId, count + activities.getOrElse(monkeyId, 0)))
  }
}
