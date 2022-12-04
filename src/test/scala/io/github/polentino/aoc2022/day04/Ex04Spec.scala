package io.github.polentino.aoc2022.day04

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex04Spec extends ZIOSpecDefault {

  private val sampleInput =
    """
      |2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin.split("\\n").filter(_.nonEmpty).toList

  override def spec =
    suite("Day 04")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex04.solveFullOverlap(sampleInput)
          assertTrue(result == 2)
        },
        test("should find the correct solution") {
          val result = Ex04.solveFullOverlap(slurp("day04ex1.txt"))
          assertTrue(result == 530)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex04.solvePartialOverlap(sampleInput)
          assertTrue(result == 4)
        },
        test("should find the correct solution") {
          val result = Ex04.solvePartialOverlap(slurp("day04ex1.txt"))
          assertTrue(result == 903)
        }
      )
    )
}
