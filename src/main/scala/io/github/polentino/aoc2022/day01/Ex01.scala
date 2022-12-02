package io.github.polentino.aoc2022.day01

object Ex01 {

  def solve(lines: List[String]): Seq[Int] = {
    val (listAcc, acc) = lines
      .foldLeft((List.empty[List[Int]], List.empty[Int]))({ case ((listAcc, acc), current) =>
        if (current.isBlank) {
          val r = listAcc :+ acc
          (r, List.empty)
        } else {
          (listAcc, acc :+ Integer.parseInt(current))
        }
      })

    (listAcc :+ acc)
      .map(_.sum)
      .sorted
      .reverse
  }
}
