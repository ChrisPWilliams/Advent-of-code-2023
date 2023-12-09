package advent.of.code.day4

import advent.of.code.utils.Solution
import cats.effect.IOApp

import scala.annotation.tailrec
import scala.math.pow


object DayFour extends Solution with IOApp.Simple {

  lazy val day = 4

  def solve(puzzleInput: String): String = {
    val partOne = solvePartOne(puzzleInput)
    val partTwo = solvePartTwo(puzzleInput)
    "Part 1 solution: " ++ partOne ++ "\nPart 2 solution: " ++ partTwo
  }

  case class ScratchCard(
                        id: Int,
                        winningNumbers: Seq[Int],
                        myNumbers: Seq[Int]
                        )

  def parseToScratchCards(input: String): Seq[ScratchCard] = {
    val cardExtractor = raw"Card +([0-9]+): +([^|]+)\| +([^|]+)".r
    input.split("\n").map {
      case cardExtractor(id, winningNumbers, myNumbers) =>
        ScratchCard(
          id = id.toInt,
          winningNumbers = winningNumbers.split(" +").map(_.toInt),
          myNumbers = myNumbers.split(" +").map(_.toInt)
        )
    }
  }

  def solvePartOne(puzzleInput: String): String = {
    val cards = parseToScratchCards(puzzleInput)
    cards.map(card =>
      card.myNumbers.count(card.winningNumbers.contains) match {
        case 0 => 0
        case matchCount => pow(2, matchCount-1).toInt
      }
    ).sum.toString
  }

  def getCardFromId(cards: Seq[ScratchCard], id: Int): Option[ScratchCard] = {
    cards.find(_.id == id)
  }

  def getNewCards(cards: Seq[ScratchCard], card: ScratchCard): Seq[ScratchCard] = {
    val matches = card.myNumbers.count(card.winningNumbers.contains)
    matches match {
      case 0 => Seq.empty
      case matchCount => ((card.id + 1) to (card.id + matchCount)).flatMap(id => getCardFromId(cards, id))
    }
  }

  @tailrec
  def addCards(currentCards: Seq[ScratchCard], id: Int, maxId: Int): Seq[ScratchCard] = {
    if (id > maxId) {
      currentCards
    } else {
      val getDuplicatesFrom = currentCards.filter(_.id == id)
      val newCards = getDuplicatesFrom.flatMap(card => getNewCards(currentCards, card))
      addCards(currentCards ++ newCards, id + 1, maxId)
    }
  }

  def solvePartTwo(puzzleInput: String): String = {
    val cards = parseToScratchCards(puzzleInput)
    addCards(cards, 1, cards.length + 1).length.toString
  }

}
