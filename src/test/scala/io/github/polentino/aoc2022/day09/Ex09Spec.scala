package io.github.polentino.aoc2022.day09

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex09Spec extends ZIOSpecDefault {

  private val sampleInput =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.split("\\n").toList

  private val sampleInput2 =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin.split("\\n").toList

  override def spec =
    suite("Day 09")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex09.solve(sampleInput)
          assertTrue(result == 13)
        },
        test("should find the correct solution") {
          val result = Ex09.solve(slurp("day09ex1.txt"))
          assertTrue(result == 6212)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input2]") {
          val result = Ex09.solve(sampleInput2, 9)
          assertTrue(result == 36)
        },
        test("should find the correct solution") {
          val result = Ex09.solve(slurp("day09ex1.txt"), 9)
          assertTrue(result == 2522)
        }
      )
    )
}
