package io.github.polentino.aoc2022.day06

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp

object Ex06Spec extends ZIOSpecDefault {

  override def spec =
    suite("Day 06")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input1]") {
          val result = Ex06.startOfMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
          assertTrue(result == 7)
        },
        test("should find the correct solution [sample input2]") {
          val result = Ex06.startOfMarker("bvwbjplbgvbhsrlpgdmjqwftvncz")
          assertTrue(result == 5)
        },
        test("should find the correct solution [sample input3]") {
          val result = Ex06.startOfMarker("nppdvjthqldpwncqszvftbrmjlhg")
          assertTrue(result == 6)
        },
        test("should find the correct solution [sample input4]") {
          val result = Ex06.startOfMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
          assertTrue(result == 10)
        },
        test("should find the correct solution [sample input5]") {
          val result = Ex06.startOfMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
          assertTrue(result == 11)
        },
        test("should find the correct solution") {
          val result = Ex06.startOfMarker(slurp("day06ex1.txt").mkString)
          assertTrue(result == 1760)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input1]") {
          val result = Ex06.startOfMessage("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
          assertTrue(result == 19)
        },
        test("should find the correct solution [sample input2]") {
          val result = Ex06.startOfMessage("bvwbjplbgvbhsrlpgdmjqwftvncz")
          assertTrue(result == 23)
        },
        test("should find the correct solution [sample input3]") {
          val result = Ex06.startOfMessage("nppdvjthqldpwncqszvftbrmjlhg")
          assertTrue(result == 23)
        },
        test("should find the correct solution [sample input4]") {
          val result = Ex06.startOfMessage("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
          assertTrue(result == 29)
        },
        test("should find the correct solution [sample input5]") {
          val result = Ex06.startOfMessage("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
          assertTrue(result == 26)
        },
        test("should find the correct solution") {
          val result = Ex06.startOfMessage(slurp("day06ex1.txt").mkString)
          assertTrue(result == 2974)
        }
      )
    )
}
