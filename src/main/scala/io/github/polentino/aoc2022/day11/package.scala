package io.github.polentino.aoc2022

import io.github.polentino.aoc2022.day11.Addend._

package object day11 {

  sealed trait Addend

  object Addend {

    def apply(str: String): Addend = str match {
      case "old" => Old
      case value => Nr(Integer.parseInt(value))
    }

    case object Old extends Addend

    final case class Nr(value: BigInt) extends Addend
  }

  sealed trait Operator {
    def execute(ls: BigInt, rs: BigInt): BigInt
  }

  object Operator {

    def apply(ch: String): Operator = ch match {
      case "+" => Plus
      case "*" => Times
    }

    case object Plus extends Operator {
      override def execute(ls: BigInt, rs: BigInt): BigInt = ls + rs
    }

    case object Times extends Operator {
      override def execute(ls: BigInt, rs: BigInt): BigInt = ls * rs
    }
  }

  final case class Operation(lsAddend: Addend, op: Operator, rsAddend: Addend) {

    def execute(old: BigInt) = {
      def fromAddend(a: Addend) = a match {
        case Old       => old
        case Nr(value) => value
      }

      val ls = fromAddend(lsAddend)
      val rs = fromAddend(rsAddend)
      op.execute(ls, rs)
    }
  }

  final case class TestCondition(divisibleBy: BigInt, ifTrue: BigInt, ifFalse: BigInt) {

    def test(input: BigInt): BigInt =
      if (input % divisibleBy == 0) {
        println(s"\t\tCurrent worry level is divisible by $divisibleBy.")
        ifTrue
      } else {
        println(s"\t\tCurrent worry level is not divisible by $divisibleBy.")
        ifFalse
      }
  }

  final case class Throw(to: BigInt, item: BigInt, originalWorry: BigInt)
}
