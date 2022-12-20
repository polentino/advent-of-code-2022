package io.github.polentino.aoc2022.day09

import io.github.polentino.aoc2022.day09.Ex09.Move._

object Ex09 {

  private val move = "([RLUD])\\s(\\d+)".r

  def solve(lines: List[String], tailSize: Int = 1) = {
    val moves = lines.map { case move(direction, steps) =>
      direction match {
        case "U" => Up(Integer.parseInt(steps))
        case "D" => Down(Integer.parseInt(steps))
        case "L" => Left(Integer.parseInt(steps))
        case "R" => Right(Integer.parseInt(steps))
      }
    }

    val acc = Accumulator(tail = List.fill(tailSize)(Point(0, 0)))
    moves.foldLeft(acc) { case (acc, move) => acc.update(move) }.visited.distinct.length
  }

  final case class Point(x: Int, y: Int)

  final case class Accumulator(
    head: Point = Point(0, 0),
    // tail: Point = Point(0, 0),
    tail: List[Point] = List(Point(0, 0)),
    visited: List[Point] = List(Point(0, 0))
  ) {

    def update(move: Move): Accumulator = move match {
      case Up(steps)    => moveWith(steps, h => h.copy(y = h.y + 1))
      case Down(steps)  => moveWith(steps, h => h.copy(y = h.y - 1))
      case Left(steps)  => moveWith(steps, h => h.copy(x = h.x - 1))
      case Right(steps) => moveWith(steps, h => h.copy(x = h.x + 1))
    }

    private def moveWith(steps: Int, moveHeadFn: Point => Point): Accumulator = {
      def loop(acc: Accumulator, steps: Int): Accumulator = {
        if (steps == 0) acc
        else {
          val newHead = moveHeadFn(acc.head)
          // val newTail = calculateTail(newHead, acc.tail)
          val newTail = acc.tail.scanLeft(newHead)(calculateTail).slice(1, acc.tail.size + 1) // ehehee

          val newAcc = acc.copy(
            head = newHead,
            tail = newTail,
            // visited = acc.visited :+ newTail
            visited = acc.visited :+ newTail.last
          )
          loop(newAcc, steps - 1)
        }
      }

      loop(this, steps)
    }

    private def calculateTail(h: Point, t: Point): Point = {
      if (isAdjacent(h, t)) t
      else if (isCrossDistant(h, t)) moveTail(h, t)
      else if (isCrossAdjacent(h, t)) t
      else moveTailDiagonally(h, t)
    }

    private def isAdjacent(h: Point, t: Point) =
      (h == t) ||
        (math.abs(h.x - t.x) == 1 && h.y == t.y) ||
        (math.abs(h.y - t.y) == 1 && h.x == t.x)

    private def isCrossAdjacent(h: Point, t: Point) =
      (math.abs(h.x - t.x) <= 1) && (math.abs(h.y - t.y) <= 1)

    private def isCrossDistant(h: Point, t: Point): Boolean =
      (math.abs(h.x - t.x) == 2 && h.y == t.y) ||
        (math.abs(h.y - t.y) == 2 && h.x == t.x)

    private def moveTail(h: Point, t: Point) =
      if (h.y == t.y) {
        val delta = math.signum(h.x - t.x)
        t.copy(x = t.x + delta)
      } else {
        val delta = math.signum(h.y - t.y)
        t.copy(y = t.y + delta)
      }

    private def moveTailDiagonally(head: Point, tail: Point) = {
      val dx = head.x - tail.x
      val dy = head.y - tail.y
      tail.copy(
        x = tail.x + math.signum(dx),
        y = tail.y + math.signum(dy)
      )
    }
  }

  sealed trait Move { val steps: Int }

  object Move {
    final case class Up(steps: Int)    extends Move
    final case class Down(steps: Int)  extends Move
    final case class Left(steps: Int)  extends Move
    final case class Right(steps: Int) extends Move
  }
}
