package day7

import scala.io.Source
import math.Ordered.orderingToOrdered

val testLines = List(
  "32T3K 765",
  "T55J5 684",
  "KK677 28",
  "KTJJT 220",
  "QQQJA 483"
)

enum HandType:
  case FiveOfKind, FourOfKind, FullHouse, ThreeOfKind, TwoPair, OnePair,
    HighCard

given Ordering[HandType] with
  def compare(h1: HandType, h2: HandType): Int =
    h2.ordinal compare h1.ordinal

import HandType._

case class Hand(cards: String):
  def handType: HandType =
    cards.groupBy(identity).values.map(_.length).toList.sorted match
      case List(5)          => FiveOfKind
      case List(1, 4)       => FourOfKind
      case List(2, 3)       => FullHouse
      case List(1, 1, 3)    => ThreeOfKind
      case List(1, 2, 2)    => TwoPair
      case List(1, 1, 1, 2) => OnePair
      case _                => HighCard

  def handType2: HandType =
    "23456789TQKA"
      .map(c => Hand(cards.replaceAll("J", c.toString)))
      .maxBy(_.handType)
      .handType

abstract class HandOrdering extends Ordering[Hand]:
  val cardRank: Map[Char, Int]
  val handType: Hand => HandType

  def compare(x: Hand, y: Hand): Int =
    handType(x) compare handType(y) match
      case 0 =>
        x.cards
          .zip(y.cards)
          .map((a, b) => cardRank(a) compare cardRank(b))
          .dropWhile(_ == 0)
          .head
      case r => r

object Pt1Ordering extends HandOrdering:
  override val cardRank = "23456789TJQKA".zipWithIndex.toMap
  override val handType = _.handType

object Pt2Ordering extends HandOrdering:
  override val cardRank = "J23456789TQKA".zipWithIndex.toMap
  override val handType = _.handType2

object Parser:
  def parseLine(line: String): (Hand, Int) =
    val items = line.split(' ')
    (Hand(items(0)), items(1).toInt)

@main def day7: Unit =
  val testPlays = testLines.map(Parser.parseLine)
  val plays =
    Source.fromFile("resources/day7.txt").getLines.map(Parser.parseLine).toList

  val sumWinnings = (ordering: Ordering[Hand]) =>
    plays
      .sortBy(_._1)(ordering)
      .zipWithIndex
      .map((p, v) => p._2 * (v + 1))
      .sum

  val pt1 = sumWinnings(Pt1Ordering)
  val pt2 = sumWinnings(Pt2Ordering)

  println(pt1)
  println(pt2)
