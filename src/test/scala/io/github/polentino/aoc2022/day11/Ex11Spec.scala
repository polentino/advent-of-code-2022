package io.github.polentino.aoc2022.day11

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex11Spec extends ZIOSpecDefault {

  private val sampleInput =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin.split("\\n").toList

  override def spec =
    suite("Day 11")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex11.solveFirst(sampleInput)
          assertTrue(result == BigInt(10605))
        },
        test("should find the correct solution") {
          val result = Ex11.solveFirst(slurp("day11ex1.txt"))
          assertTrue(result == BigInt(121450))
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex11.solveSecond(sampleInput)
          assertTrue(result == BigInt(10605))
        },
        test("should find the correct solution") {
          val result = Ex11.solveSecond(slurp("day11ex1.txt"))
          assertTrue(result == BigInt(10605))
        }
      )
    )
}
