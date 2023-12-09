package advent.of.code.day3

import advent.of.code.utils.Solution
import cats.effect.IOApp

import scala.annotation.tailrec
import scala.util.matching.Regex


object DayThree extends Solution with IOApp.Simple {

  lazy val day = 3

  def solve(puzzleInput: String): String = {
    val partOne = solvePartOne(puzzleInput)
    val partTwo = solvePartTwo(puzzleInput)
    "Part 1 solution: " ++ partOne ++ "\nPart 2 solution: " ++ partTwo
  }

  object Regexes {
    val GetFullNum: Regex = "[0-9]+".r
    val p1GetSymbol: Regex = "[^a-zA-Z0-9.]".r
    val p2ForwardAdjacentNum: Regex = "..[0-9]".r
    val p2BackwardAdjacentNum: Regex = "[0-9]..".r
    val p2GetNum: Regex = "(?<=[0-9])[0-9]*".r
  }

  def getHeightAndWidthAndBuffer(input: String): (Int, Int, String) = {
    val rows = input.split("\n")
    val height = rows.length + 2
    val width = rows.head.length + 2
    val bufferedSchematic = "." * width ++ rows.map(row => "." ++ row ++ ".").mkString ++ "." * width
    (height, width, bufferedSchematic)
  }

  @tailrec
  def getParts(
                startY: Int,
                schematic: String,
                parts: Seq[Int],
                width: Int,
                height: Int
              ): Seq[Int] = {
    if (startY == height - 1) {
      parts
    } else {
      val adjacentRows = for {
        (x, i) <- schematic.zipWithIndex
        if ((i / width) - startY >= -1) &&
          ((i / width) - startY <= 1)
      } yield x
      val newParts = addPartsInRow(
        startX = 1,
        rows = adjacentRows.mkString,
        parts = parts,
        width = width
      )
      getParts(startY = startY + 1,
        schematic = schematic,
        parts = newParts,
        width = width,
        height = height)
    }
  }

  @tailrec
  def addPartsInRow(startX: Int, rows: String, parts: Seq[Int], width: Int): Seq[Int] = {
    if (startX == width - 1) {
      parts
    } else {
      val char = rows(width + startX)
      if (char.isDigit) {
        val number = Regexes.GetFullNum.findFirstIn(rows.slice(width + startX, width * 2)).get
        val checkSpace = rows.slice(startX-1, startX+1 + number.length) ++
          rows.slice(startX-1 + width, startX+1 + width + number.length) ++
          rows.slice(startX-1 + width * 2, startX+1 + number.length + width * 2)
        Regexes.p1GetSymbol.findFirstIn(checkSpace) match {
          case None => addPartsInRow(
            startX = startX + number.length,
            rows = rows,
            parts = parts,
            width = width)
          case _ => addPartsInRow(
            startX = startX + number.length,
            rows = rows,
            parts = number.toInt +: parts,
            width = width
          ) // we only add the new part to our list if the adjacent spaces contain symbols
        }
      } else {
        addPartsInRow(
          startX = startX + 1,
          rows = rows,
          parts = parts,
          width = width)
      }
    }
  }

  def solvePartOne(puzzleInput: String): String = {
    val (height, width, bufferedSchematic) = getHeightAndWidthAndBuffer(puzzleInput)
    val parts = getParts(
      height = height,
      width = width,
      schematic = bufferedSchematic,
      startY = 1,
      parts = Seq.empty
    )
    parts.sum.toString
  }


  @tailrec
  def getGearRatios(
                     startX: Int,
                     startY: Int,
                     schematic: String,
                     gears: Seq[Int],
                     width: Int,
                     height: Int
                   ): Seq[Int] = {
    if (startX % width == width - 1) {
      if (startY == height - 2) {
        gears
      } else {
        getGearRatios(
          startX = 1,
          startY = startY + 1,
          schematic = schematic,
          gears = gears,
          width = width,
          height = height
        )
      }
    } else {
      val char = schematic(width * startY + startX)
      if (char == '*') {
        val adjacentTop = schematic.slice((startX - 1) + (startY - 1) * width, (startX + 2) + (startY - 1) * width)
        val adjacentMid = schematic.slice((startX - 1) + startY * width, (startX + 2) + startY * width)
        val adjacentBottom = schematic.slice((startX - 1) + (startY + 1) * width, (startX + 2) + (startY + 1) * width)

        val topRowForward = Regexes.p2ForwardAdjacentNum.findFirstIn(adjacentTop) match {
          case None => ""
          case _ => Regexes.p2GetNum.findFirstIn(
            schematic.slice((startX + 1) + (startY - 1) * width, startY * width)
          ).get
        }
        val topRowBackward = Regexes.p2BackwardAdjacentNum.findFirstIn(adjacentTop) match {
          case None => ""
          case _ => Regexes.p2GetNum.findFirstIn(
            schematic.slice((startY - 1) * width, (startY - 1) * width + startX).reverse
          ).get.reverse
        }
        val midRowForward = Regexes.p2ForwardAdjacentNum.findFirstIn(adjacentMid) match {
          case None => ""
          case _ => Regexes.p2GetNum.findFirstIn(
            schematic.slice((startX + 1) + startY * width, (startY + 1) * width)
          ).get
        }
        val midRowBackward = Regexes.p2BackwardAdjacentNum.findFirstIn(adjacentMid) match {
          case None => ""
          case _ => Regexes.p2GetNum.findFirstIn(
            schematic.slice(startY * width, startY * width + startX).reverse
          ).get.reverse
        }
        val bottomRowForward = Regexes.p2ForwardAdjacentNum.findFirstIn(adjacentBottom) match {
          case None => ""
          case _ => Regexes.p2GetNum.findFirstIn(
            schematic.slice((startX + 1) + (startY + 1) * width, (startY + 2) * width)
          ).get
        }
        val bottomRowBackward = Regexes.p2BackwardAdjacentNum.findFirstIn(adjacentBottom) match {
          case None => ""
          case _ => Regexes.p2GetNum.findFirstIn(
            schematic.slice((startY + 1) * width, (startY + 1) * width + startX).reverse
          ).get.reverse
        }
        val topRowFull = topRowBackward ++ adjacentTop ++ topRowForward
        val midRowFull = midRowBackward ++ adjacentMid ++ midRowForward
        val bottomRowFull = bottomRowBackward ++ adjacentBottom ++ bottomRowForward

        val matches: List[String] = Regexes.GetFullNum.findAllIn(topRowFull).toList ++
          Regexes.GetFullNum.findAllIn(midRowFull).toList ++
          Regexes.GetFullNum.findAllIn(bottomRowFull).toList
        if (matches.length == 2) {
          val ratio = matches.map(_.toInt).product
          getGearRatios(
            startX = startX + 1,
            startY = startY,
            schematic = schematic,
            gears = ratio +: gears,
            width = width,
            height = height
          )
        } else {
          getGearRatios(
            startX = startX + 1,
            startY = startY,
            schematic = schematic,
            gears = gears,
            width = width,
            height = height
          )
        }
      } else {
        getGearRatios(
          startX = startX + 1,
          startY = startY,
          schematic = schematic,
          gears = gears,
          width = width,
          height = height
        )
      }
    }
  }

  def solvePartTwo(puzzleInput: String): String = {
    val (height, width, bufferedSchematic) = getHeightAndWidthAndBuffer(puzzleInput)
    val gearRatios = getGearRatios(
      height = height,
      width = width,
      schematic = bufferedSchematic,
      startX = 1,
      startY = 1,
      gears = Seq.empty
    )
    gearRatios.sum.toString
  }

}
