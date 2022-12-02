package io.github.polentino.aoc2022.day01

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex01Spec extends ZIOSpecDefault {

  private val sampleInput = """
    |1000
    |2000
    |3000
    |
    |4000
    |
    |5000
    |6000
    |
    |7000
    |8000
    |9000
    |
    |10000""".stripMargin.split("\\n").toList

  override def spec =
    suite("Day 01")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex01.solve(sampleInput).head
          assertTrue(result == 24000)
        },
        test("should find the correct solution") {
          val result = Ex01.solve(slurp("day01ex1.txt")).head
          assertTrue(result == 69289)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex01.solve(sampleInput)
          assertTrue(result.take(3).sum == 45000)
        },
        test("should find the correct solution") {
          val result = Ex01.solve(slurp("day01ex1.txt"))
          assertTrue(result.take(3).sum == 205615)
        }
      )
    )
}
