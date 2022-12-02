package io.github.polentino.aoc2022.day02

object Ex02 {

  enum Move {
    case Rock, Paper, Scissor
  }

  final case class MovesMapping(rock: Char, paper: Char, scissor: Char) {

    def decode(char: Char): Move = char match {
      case `rock`    => Move.Rock
      case `paper`   => Move.Paper
      case `scissor` => Move.Scissor
    }
  }

  private val myMapping       = MovesMapping('X', 'Y', 'Z')
  private val opponentMapping = MovesMapping('A', 'B', 'C')

  private def score(opponentMove: Move, myMove: Move): Int = myMove.ordinal - opponentMove.ordinal match {
    case 0  => myMove.ordinal + 4 // it's a draw
    case -2 => myMove.ordinal + 7 // I won (rock vs scissor)
    case -1 => myMove.ordinal + 1 // I lost (rock vs paper) or (paper vs scissor)
    case 2  => myMove.ordinal + 1 // I lost (scissor vs rock)
    case 1  => myMove.ordinal + 7 // I won (scissor vs paper) or (paper vs rock)
  }

  private def losingMove(opponentMove: Move): Move = opponentMove match {
    case Move.Rock    => Move.Scissor
    case Move.Paper   => Move.Rock
    case Move.Scissor => Move.Paper
  }

  private def winningMove(opponentMove: Move): Move = opponentMove match {
    case Move.Rock    => Move.Paper
    case Move.Paper   => Move.Scissor
    case Move.Scissor => Move.Rock
  }

  private def counterMove(opponentMove: Move, strategy: Char): Move = strategy match {
    case 'X' => losingMove(opponentMove)  // I need to lose
    case 'Y' => opponentMove              // draw
    case 'Z' => winningMove(opponentMove) // I need to win
  }

  def totalScore(lines: List[String]): Int = {
    lines.map(line => {
      score(opponentMapping.decode(line.charAt(0)), myMapping.decode(line.charAt(2)))
    }).sum
  }

  def totalScore2(lines: List[String]): Int = {
    lines.map(line => {
      val opponentMove = opponentMapping.decode(line.charAt(0))
      val myMove       = counterMove(opponentMove, line.charAt(2))
      score(opponentMove, myMove)
    }).sum
  }
}
