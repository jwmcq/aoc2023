package day5

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source

val test = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

val getDigits: (String => List[BigInt]) =
  "\\d+".r.findAllIn(_).map(BigInt(_)).toList

enum ParsedLine:
  case Blank
  case Title(title: String)
  case Ints(ints: List[BigInt])

def parseLine(line: String): ParsedLine =
  if line == "" then ParsedLine.Blank
  else {
    val ds = getDigits(line)
    if ds.isEmpty then ParsedLine.Title(line) else ParsedLine.Ints(ds)
  }

def parseMappings(
    lines: List[String]
): (List[BigInt], ListMap[String, List[Mapping]]) =
  val seeds = getDigits(lines.head)

  @tailrec
  def loop(
      lines: List[String],
      currentTitle: String,
      mappings: ListMap[String, List[Mapping]]
  ): ListMap[String, List[Mapping]] =
    if lines.isEmpty then mappings
    else
      parseLine(lines.head) match
        case ParsedLine.Blank    => loop(lines.tail, currentTitle, mappings)
        case ParsedLine.Title(s) => loop(lines.tail, s, mappings)
        case ParsedLine.Ints(ints) =>
          loop(
            lines.tail,
            currentTitle,
            mappings + (currentTitle -> (Mapping(
              ints(0),
              ints(1),
              ints(2)
            ) :: mappings.getOrElse(currentTitle, List())))
          )

  (seeds, loop(lines.tail, "", ListMap()))

case class Mapping(destStart: BigInt, sourceStart: BigInt, length: BigInt) {
  val destRange = destStart to (destStart + length - 1)
  val sourceRange = sourceStart to (sourceStart + length - 1)

  infix def contains(n: BigInt): Boolean =
    sourceRange contains n

  def search(n: BigInt): Option[BigInt] =
    this contains n match
      case true  => Some(destStart + (n - sourceStart))
      case false => None
}

@tailrec
def traverseMappings(n: BigInt, mappings: List[List[Mapping]]): BigInt =
  mappings match
    case Nil => n
    case x :: xs => {
      val v = x.flatMap(_.search(n))
      v match
        case head :: _ => traverseMappings(head, xs)
        case Nil       => traverseMappings(n, xs)
    }

@main def foo: Unit =
  val testInput = test.split("\n").toList
  val input = Source.fromFile("resources/day5.txt").getLines.toList
  val (seeds, mappings) = parseMappings(input)

  val locations = seeds.map(traverseMappings(_, mappings.values.toList))

  println(locations.min)

  println("foo")

  val pt2seeds =
    seeds.grouped(2).flatMap(x => (x(0) to (x(0) + x(1) - 1)))
  // literally just yoloing this
  val pt2dests = pt2seeds.map(traverseMappings(_, mappings.values.toList))
  println(pt2dests.min)
  println("barrrrr")
