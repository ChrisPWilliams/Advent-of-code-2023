package advent.of.code.day3

import advent.of.code.utils.Solution
import cats.effect.IOApp

import scala.annotation.tailrec


object DayThree extends Solution with IOApp.Simple {

  lazy val day = 3

  def solve(puzzleInput: String): String = {
    val partOne = solvePartOne(puzzleInput)
    val partTwo = "" //solvePartTwo(puzzleInput)
    "Part 1 solution: " ++ partOne ++ "\nPart 2 solution: " ++ partTwo
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
        val number = "[0-9]+".r.findFirstIn(rows.slice(width + startX, width * 2)).get
        val checkSpace = rows.slice(startX-1, startX+1 + number.length) ++
          rows.slice(startX-1 + width, startX+1 + width + number.length) ++
          rows.slice(startX-1 + width * 2, startX+1 + number.length + width * 2)
        "[^a-zA-Z0-9.]".r.findFirstIn(checkSpace) match {
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

}
