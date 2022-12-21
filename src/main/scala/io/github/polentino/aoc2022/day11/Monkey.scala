package io.github.polentino.aoc2022.day11

final case class Monkey(
  id: Int,
  startingItems: List[BigInt],
  operation: Operation,
  test: TestCondition,
  worryLevelFn: BigInt => BigInt
) {

  def inspect = {
    println(s"Monkey $id:")
    startingItems.map { wl =>
      println(s"\tMonkey inspects an item with a worry level of $wl.")
      val newWL        = operation.execute(wl)
      println(s"\t\tNew worry level of $newWL.")
      val newWLDivided = worryLevelFn(newWL)
      println(s"\t\tMonkey gets bored with item. Worry level is divided by 3 to $newWLDivided.")
      val monkeyId     = test.test(newWLDivided)
      println(s"\t\tItem with worry level $newWLDivided is thrown to monkey $monkeyId.")
      Throw(monkeyId, newWLDivided, wl)
    }
  }
}

object Monkey {

  private val idRx            = "Monkey (\\d+):".r
  private val startingItemsRx = "\\s+Starting items: (\\p{Print}+)".r
  private val operationRx     = "\\s+Operation: new = (old|\\d+)\\s([*+\\-/])\\s(old|\\d+)".r
  private val testRx          = "\\s+Test: divisible by (\\d+)".r
  private val ifTrueRx        = "\\s+If true: throw to monkey (\\d+)".r
  private val ifFalseRx       = "\\s+If false: throw to monkey (\\d+)".r

  def apply(lines: List[String], worryLevelFn: BigInt => BigInt): Monkey = Monkey(
    id = getId(lines(0)),
    startingItems = getStartingItems(lines(1)),
    operation = getOperation(lines(2)),
    test = getTest(lines.drop(3)),
    worryLevelFn = worryLevelFn
  )

  private def getId(str: String): Int = str match {
    case idRx(id) => Integer.parseInt(id)
  }

  private def getStartingItems(str: String): List[BigInt] = str match {
    case startingItemsRx(list) => list.split(",").map(s => BigInt(s.trim)).toList
  }

  private def getOperation(str: String): Operation = str match {
    case operationRx(ls, op, rs) => Operation(
        lsAddend = Addend(ls),
        op = Operator(op),
        rsAddend = Addend(rs)
      )
  }

  private def getTest(lines: List[String]) =
    TestCondition(
      divisibleBy = getDivisibleBy(lines(0)),
      ifTrue = getIfTrue(lines(1)),
      ifFalse = getIfFalse(lines(2))
    )

  private def getDivisibleBy(str: String) = str match {
    case testRx(value) => Integer.parseInt(value)
  }

  private def getIfTrue(str: String) = str match {
    case ifTrueRx(value) => Integer.parseInt(value)
  }

  private def getIfFalse(str: String) = str match {
    case ifFalseRx(value) => Integer.parseInt(value)
  }
}
