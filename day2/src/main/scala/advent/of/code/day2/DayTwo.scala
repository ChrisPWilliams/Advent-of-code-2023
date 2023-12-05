package advent.of.code.day2

import advent.of.code.utils.Solution
import cats.effect.IOApp


object DayTwo extends Solution with IOApp.Simple {

  lazy val day = 2

  def solve(puzzleInput: String): String = {
    val partOne = solvePartOne(puzzleInput)
    val partTwo = "" //solvePartTwo(puzzleInput)
    "Part 1 solution: " ++ partOne ++ "\nPart 2 solution: " ++ partTwo
  }

  case class Game(
                   id: Int,
                   rounds: Seq[GameRound]
                 )

  case class GameRound (
                       red: Option[Int],
                       blue: Option[Int],
                       green: Option[Int]
                       )

  def parseGames(puzzleInput: String): Seq[Game] = {
    val gameExtractor = "Game ([0-9]+): (.+)".r
    val redExtractor = "([0-9]+(?= red))".r
    val blueExtractor = "([0-9]+(?= blue))".r
    val greenExtractor = "([0-9]+(?= green))".r
    puzzleInput.split("\n").map {
      case gameExtractor(id, roundStr) =>
        Game(
          id = id.toInt,
          rounds = roundStr.split(";").map(round =>
            GameRound(
              red = redExtractor.findFirstIn(round).map(_.toInt),
              blue = blueExtractor.findFirstIn(round).map(_.toInt),
              green = greenExtractor.findFirstIn(round).map(_.toInt)
            )
          )
        )
    }
  }

  def solvePartOne(str: String): String = {
    val games = parseGames(str)
    val (redLimit, blueLimit, greenLimit) = (12, 14, 13)

    games.filter(game =>
      game.rounds.map(round =>
        round.red.getOrElse(0) <= redLimit &&
        round.blue.getOrElse(0) <= blueLimit &&
        round.green.getOrElse(0) <= greenLimit
      ).reduce(_&&_)
    ).map(_.id).sum.toString

  }
}
