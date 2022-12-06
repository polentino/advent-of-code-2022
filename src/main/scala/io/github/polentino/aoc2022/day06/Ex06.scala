package io.github.polentino.aoc2022.day06

object Ex06 {

  def startOfMarker(line: String, size: Int = 4): Int =
    line.sliding(size, 1)
      .zipWithIndex
      .toList
      .find(_._1.toSet.size == size)
      .map(_._2 + size)
      .getOrElse(Int.MinValue) // shouldn't happen, but just in case ...

  def startOfMessage(line: String): Int = startOfMarker(line, 14)
}
