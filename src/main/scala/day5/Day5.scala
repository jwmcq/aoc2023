package day5

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.parallel.CollectionConverters._
import scala.io.Source

val getDigits: (String => List[BigInt]) =
  "\\d+".r.findAllIn(_).map(BigInt(_)).toList

enum ParsedLine:
  case Blank
  case Title(title: String)
  case Ints(ints: List[BigInt])

case class Mapping(destStart: BigInt, sourceStart: BigInt, length: BigInt) {
  val destRange = destStart to (destStart + length - 1)
  val sourceRange = sourceStart to (sourceStart + length - 1)

  def search(n: BigInt): Option[BigInt] =
    sourceRange contains n match
      case true  => Some(destStart + (n - sourceStart))
      case false => None
}

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
  val input = Source.fromFile("resources/day5.txt").getLines.toList
  val (seeds, mappings) = parseMappings(input)

  val locations = seeds.map(traverseMappings(_, mappings.values.toList))

  val start = System.currentTimeMillis

  println("part 1:")
  println(locations.min)

  val pt2seeds =
    seeds.grouped(2).flatMap(x => (x(0) to (x(0) + x(1) - 1)))
  // literally just yoloing this
  val pt2locations =
    pt2seeds.toList.par.map(traverseMappings(_, mappings.values.toList))

  println("part 2:")
  println(pt2locations.min)

  val finish = System.currentTimeMillis - start

  println(s"took $finish")
  // took 18620209ms without par
