package day7

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
