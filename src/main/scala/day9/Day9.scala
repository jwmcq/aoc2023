package day9

import scala.io.Source

val testInput = List(
  "0 3 6 9 12 15",
  "1 3 6 10 15 21",
  "10 13 16 21 30 45"
)

extension (i: Int) def |-|(j: Int): Int = (i - j).abs

def diffs(ints: Seq[Int]): Seq[Int] =
  ints.sliding(2).map(_.reduce((a, b) => b - a)).toSeq

def exhaustDiffs(ints: Seq[Int]): LazyList[Seq[Int]] =
  LazyList.iterate(ints)(diffs).takeWhile(s => !(s.forall(_ == 0)))

def parseInput(lines: Seq[String]): Seq[Seq[Int]] =
  lines.map(_.split(" ").map(_.toInt))

@main def day9: Unit =
  val input = testInput
  val intSeqs = parseInput(input)
  println(exhaustDiffs(intSeqs(0)).toList)
  println("foo")
