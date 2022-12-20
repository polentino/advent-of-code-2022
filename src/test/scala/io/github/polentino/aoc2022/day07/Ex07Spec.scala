package io.github.polentino.aoc2022.day07

import zio.test.*

import io.github.polentino.aoc2022.TestUtils.slurp
import io.github.polentino.aoc2022.day07.Ex07.FS.*
import io.github.polentino.aoc2022.day07.Ex07.TreeBuilder

object Ex07Spec extends ZIOSpecDefault {

  private val sampleInput =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin.split("\\n").toList

  override def spec =
    suite("Day 07")(
      suite("Exercise 1")(
        test("should find the correct solution [sample input]") {
          val result = Ex07.solve(sampleInput)
          assertTrue(result == 95437)
        },
        test("should find the correct solution") {
          val result = Ex07.solve(slurp("day07ex1.txt"))
          assertTrue(result == 1783610)
        }
      ),
      suite("Exercise 2")(
        test("should find the correct solution [sample input]") {
          val result = Ex07.solve2(sampleInput)
          assertTrue(result == 24933642)
        },
        test("should find the correct solution") {
          val result = Ex07.solve2(slurp("day07ex1.txt"))
          assertTrue(result == 4370655)
        }
      )
    )
}
