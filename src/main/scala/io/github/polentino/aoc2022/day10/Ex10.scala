package io.github.polentino.aoc2022.day10

object Ex10 {

  private val addx = "addx\\s(-?\\d+)".r
  private val noop = "noop\\s?".r

  def solve(lines: List[String]) = {
    val cpu = CPU(desiredCycles = List(20, 60, 100, 140, 180, 220))
    lines.foldLeft(cpu)((cpu, line) => cpu.executeInstruction(line))
  }

  case class CPU(
    x: Int = 1,
    cycle: Int = 1,
    desiredCycles: List[Int] = List.empty,
    signalStrength: List[Int] = List.empty,
    sprite: String = "###.....................................",
    image: List[String] = List.empty,
    buffer: String = ""
  ) {
    def totalStrength = signalStrength.sum
    def totalImage    = (image :+ buffer).mkString("\n")

    def executeInstruction(line: String) = line match {
      case addx(value) =>
        val c           = cycle + 1
        val newStrength =
          if (desiredCycles.contains(c)) {
            signalStrength :+ (c * x)
          } else if (desiredCycles.contains(cycle)) {
            signalStrength :+ (cycle * x)
          } else signalStrength

        val (newImage, newBuffer)   = createImage(cycle - 1, sprite, image, buffer)
        val (newImage2, newBuffer2) = createImage(cycle, sprite, newImage, newBuffer)
        val newSprite               = updateSprite(Integer.parseInt(value), sprite)
        val newX                    = x + Integer.parseInt(value)

        println(
          s"""Sprite position: $sprite
             |
             |Start cycle   ${cycle}: begin executing addx $value
             |During cycle  ${cycle}: CRT draws pixel in position ${cycle - 1}
             |Current CRT row: $newBuffer
             |
             |During cycle   ${cycle + 1}: CRT draws pixel in position ${cycle}
             |Current CRT row: $newBuffer2
             |End of cycle  ${cycle + 1}: finish executing addx $value (Register X is now $newX)""".stripMargin
        )

        copy(
          x = newX,
          cycle = c + 1,
          signalStrength = newStrength,
          sprite = newSprite,
          image = newImage2,
          buffer = newBuffer2
        )

      case noop() =>
        val newStrength           =
          if (desiredCycles.contains(cycle)) {
            signalStrength :+ (cycle * x)
          } else signalStrength
        val (newImage, newBuffer) = createImage(cycle - 1, sprite, image, buffer)

        println(
          s"""Sprite position: $sprite
             |
             |Start cycle   ${cycle}: begin executing noop
             |During cycle  ${cycle}: CRT draws pixel in position ${cycle - 1}
             |Current CRT row: $newBuffer
             |End of cycle  ${cycle}: finish executing noop""".stripMargin
        )

        copy(cycle = cycle + 1, signalStrength = newStrength, image = newImage, buffer = newBuffer)
    }

    private def createImage(cycle: Int, sprite: String, image: List[String], buffer: String) = {
      val idx = cycle % 40
      if (buffer.length == 40) {
        (image :+ buffer, "" + sprite(idx))
      } else {
        (image, buffer + sprite(idx))
      }
    }

    private def updateSprite(i: Int, str: String) = {
      if (i > 0) {
        val (l, r) = str.splitAt(str.length - i)
        r + l
      } else {
        val (l, r) = str.splitAt(math.abs(i))
        r + l
      }
    }
  }
}
