package advent.of.code.day1

import advent.of.code.utils.Solution
import cats.effect.IOApp

object DayOne extends Solution with IOApp.Simple {

  lazy val day = 1

  def solve(puzzleInput: String): String = {
    val partOne = solvePartOne(puzzleInput)
    val partTwo = solvePartTwo(puzzleInput)
    "Part 1 solution: " ++ partOne ++ "\nPart 2 solution: " ++ partTwo
  }

  def solvePartOne(puzzleInput: String): String = {

    def getValue(scrambledCode: String): Int = {
      val digits = scrambledCode.filter(_.isDigit)
      (digits.head.toString ++ digits.takeRight(1)).toInt
    }

    puzzleInput.split("\n")
      .map(getValue).sum.toString
  }

  def solvePartTwo(puzzleInput: String): String = {
    def getValue(scrambledCode: String): Int = {
      val patternString = "one|two|three|four|five|six|seven|eight|nine"

      def replaceDigit(pattern: String, str: String, reverse: Boolean): Option[String] = {
        {
          if (reverse) {
            val reversePattern = ("^[^0-9]*(" ++ pattern.reverse ++ ")").r
            reversePattern.findFirstIn(str.reverse).flatMap(pattern.reverse.r.findFirstIn).map(_.reverse)
          } else {
            val forwardPattern = ("^[^0-9]*(" ++ pattern ++ ")").r
            forwardPattern.findFirstIn(str).flatMap(pattern.r.findFirstIn)
          }
        } match {
          case Some("one") => Some("1")
          case Some("two") => Some("2")
          case Some("three") => Some("3")
          case Some("four") => Some("4")
          case Some("five") => Some("5")
          case Some("six") => Some("6")
          case Some("seven") => Some("7")
          case Some("eight") => Some("8")
          case Some("nine") => Some("9")
          case _ => None
        }
      }

      val replacedFirst = replaceDigit(patternString, scrambledCode, false)
      val replacedLast = replaceDigit(patternString, scrambledCode, true)

      val otherDigits = scrambledCode.filter(_.isDigit)

      (replacedFirst.getOrElse(otherDigits.head.toString) ++
        replacedLast.getOrElse(otherDigits.takeRight(1))).toInt

    }
    puzzleInput.split("\n")
      .map(getValue).sum.toString
  }

}
