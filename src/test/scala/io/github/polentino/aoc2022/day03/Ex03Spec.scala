package io.github.polentino.aoc2022.day03

import zio.test._

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex03Spec extends ZIOSpecDefault {

  private val sampleInput =
    """
      |vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.split("\\n").filter(_.nonEmpty).toList

  override def spec =
    suite("Day 03")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex03.sumOfPriorities(sampleInput)
          assertTrue(result == 157)
        },
        test("should find the correct solution") {
          val result = Ex03.sumOfPriorities(slurp("day03ex1.txt"))
          assertTrue(result == 7863)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex03.sumOfGroupedPriorities(sampleInput)
          assertTrue(result == 70)
        },
        test("should find the correct solution") {
          val result = Ex03.sumOfGroupedPriorities(slurp("day03ex1.txt"))
          assertTrue(result == 2488)
        }
      )
    )
}
