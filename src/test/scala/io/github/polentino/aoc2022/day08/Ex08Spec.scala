package io.github.polentino.aoc2022.day08

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex08Spec extends ZIOSpecDefault {

  private val sampleInput =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin.split("\\n").toList

  override def spec =
    suite("Day 08")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex08.solve(sampleInput)
          assertTrue(result == 21)
        },
        test("should find the correct solution") {
          val result = Ex08.solve(slurp("day08ex1.txt"))
          assertTrue(result == 1688)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex08.solve2(sampleInput)
          assertTrue(result == 8)
        },
        test("should find the correct solution") {
          val result = Ex08.solve2(slurp("day08ex1.txt"))
          assertTrue(result == 410400)
        }
      )
    )
}
