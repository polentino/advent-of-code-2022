package io.github.polentino.aoc2022.day02

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex02Spec extends ZIOSpecDefault {

  private val sampleInput =
    """
      |A Y
      |B X
      |C Z""".stripMargin.split("\\n").filter(_.nonEmpty).toList

  override def spec =
    suite("Day 02")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex02.totalScore(sampleInput)
          assertTrue(result == 15)
        },
        test("should find the correct solution") {
          val result = Ex02.totalScore(slurp("day02ex1.txt"))
          assertTrue(result == 15632)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex02.totalScore2(sampleInput)
          assertTrue(result == 12)
        },
        test("should find the correct solution") {
          val result = Ex02.totalScore2(slurp("day02ex1.txt"))
          assertTrue(result == 14416)
        }
      )
    )
}
