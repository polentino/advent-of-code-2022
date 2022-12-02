package io.github.polentino.aoc2022

import scala.io._

object TestUtils {
  def slurp(fileName: String): List[String] = Source.fromResource(fileName).getLines().toList
}
