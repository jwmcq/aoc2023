package day9

import scala.io.Source

// question - is it possible to make Sandreadings a subtype of Seq[Int]?
// this would make the code a lot nicer (less having to build intermediate
// SandReadings objects)
// maybe there's some sort of trait I should use but not sure what/how
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

  // legit just copy paste lol
  def fillFirstVal: SandReadings =
    def loop(seqs: Seq[SandReadings]): Seq[Int] =
      seqs.isEmpty match
        case true => Seq(0)
        case false =>
          (seqs.head.ints.head - loop(seqs.tail).head) +: seqs.head.ints
    SandReadings(loop(this.exhaustDiffs))

def parseInput(lines: Seq[String]): Seq[SandReadings] =
  lines.map(_.split(" ").map(_.toInt)).map(SandReadings(_))

@main def day9: Unit =
  val input = Source.fromFile("resources/day9.txt").getLines.toSeq
  val intSeqs = parseInput(input)
  println(intSeqs.map(_.fillLastVal.ints.last).sum)
  println(intSeqs.map(_.fillFirstVal.ints.head).sum)

  println("foo")
