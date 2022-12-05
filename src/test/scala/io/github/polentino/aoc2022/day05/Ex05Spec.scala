package io.github.polentino.aoc2022.day05

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex05Spec extends ZIOSpecDefault {

  private val sampleInput =
    """    [D]    |
      |[N] [C]    |
      |[Z] [M] [P]
      | 1   2   3
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin.split("\\n").toList

  override def spec =
    suite("Day 05")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex05.solveSingle(sampleInput)
          assertTrue(result == "CMZ")
        },
        test("should find the correct solution") {
          val result = Ex05.solveSingle(slurp("day05ex1.txt"))
          assertTrue(result == "QGTHFZBHV")
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex05.solveMultiple(sampleInput)
          assertTrue(result == "MCD")
        },
        test("should find the correct solution") {
          val result = Ex05.solveMultiple(slurp("day05ex1.txt"))
          assertTrue(result == "MGDMPSZTM")
        }
      )
    )
}
