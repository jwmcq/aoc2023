package day7

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

case class Hand(hand: String) {
  def handType: HandType =
    hand.groupBy(identity).values.map(_.length).toList.sorted match
      case List(5)          => FiveOfKind
      case List(1, 4)       => FourOfKind
      case List(2, 3)       => FullHouse
      case List(1, 1, 3)    => ThreeOfKind
      case List(1, 2, 2)    => TwoPair
      case List(1, 1, 1, 2) => OnePair
      case _                => HighCard
}

given Ordering[Hand] with
  def compare(x: Hand, y: Hand): Int =
    x.handType compare y.handType match
      case 0 => ???
