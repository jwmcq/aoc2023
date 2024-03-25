package day9

import scala.io.Source

val testInput = List(
  "0 3 6 9 12 15",
  "1 3 6 10 15 21",
  "10 13 16 21 30 45"
)

extension (i: Int) def |-|(j: Int): Int = (i - j).abs

// question - is it possible to make Sandreadings a subtype of Seq[Int]?
// this would make the code a lot nicer
case class SandReadings(ints: Seq[Int]):
  def diffs: SandReadings =
    SandReadings(ints.sliding(2).map(_.reduce((a, b) => b - a)).toSeq)

  def exhaustDiffs: LazyList[SandReadings] =
    LazyList.iterate(this)(_.diffs).takeWhile(s => !(s.ints.forall(_ == 0)))

  def fillLastVal: SandReadings =
    def loop(seqs: Seq[SandReadings]): Seq[Int] =
      seqs.isEmpty match
        case true => Seq(0)
        case false =>
          seqs.head.ints :+ (seqs.head.ints.last + loop(seqs.tail).last)
    SandReadings(loop(this.exhaustDiffs))

def parseInput(lines: Seq[String]): Seq[SandReadings] =
  lines.map(_.split(" ").map(_.toInt)).map(SandReadings(_))

@main def day9: Unit =
  // val input = testInput
  val input = Source.fromFile("resources/day9.txt").getLines.toSeq
  val intSeqs = parseInput(input)
  println(intSeqs.map(_.fillLastVal.ints.last).sum)

  println("fo")
