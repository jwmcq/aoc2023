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

case class Hand(hand: String) {
  def handType: HandType =
    ???
}
