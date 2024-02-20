package day5

import scala.collection.immutable.ListMap

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

val getDigits: (String => List[Int]) = "\\d+".r.findAllIn(_).map(_.toInt).toList

enum ParsedLine:
  case Blank
  case Title(title: String)
  case Ints(ints: List[Int])

def parseLine(line: String): ParsedLine =
  if line == "" then ParsedLine.Blank
  else {
    val ds = getDigits(line)
    if ds.isEmpty then ParsedLine.Title(line) else ParsedLine.Ints(ds)
  }

def parseMappings(
    lines: List[String]
): (List[Int], ListMap[String, List[Mapping]]) =
  val seeds = getDigits(lines.head)

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

case class Mapping(destStart: Int, sourceStart: Int, length: Int) {
  val destRange = destStart to (destStart + length)
  val sourceRange = sourceStart to (sourceStart + length)

  infix def contains(n: Int): Boolean =
    sourceRange contains n

  def map(n: Int): Int =
    // println(s"yarr: $n")
    this contains n match
      case true  => destStart + (n - sourceStart)
      case false => n

  def map2(n: Int): Option[Int] =
    this contains n match
      case true  => Some(destStart + (n - sourceStart))
      case false => None
}

def applyMappings(n: Int, mappings: List[Mapping]): Int =
  mappings
    .map(_.map2(n))
    .foldLeft(n)((acc, x) =>
      x match
        case Some(int) => int
        case None      => n
    )

def traverseMappings(n: Int, mappings: List[List[Mapping]]): Int =
  mappings.foldLeft(n)((acc: Int, x: List[Mapping]) => {
    val yarr = x.foldLeft(acc)((acc2: Int, x2: Mapping) => {
      val foo = x2.map2(acc2)
      foo match
        case Some(_) => foo
        case None    => acc2
    })
    yarr match
      case Some[Int](y) => y
      case None         => acc
  })

// def traverseMappings(n: Int, mappings: List[List[Mapping]]) =
//   mappings.

// def traverseMappings2(
//     seed: Int,
//     mappings: ListMap[String, List[Mapping]]
// ): Int =

@main def foo: Unit =
  val (seeds, mappings) = parseMappings(test.split("\n").toList)
  val ns =
    mappings.values.map(_.foldLeft(seeds.head)((acc, x) => x.map(acc)))

  var seed = 79
  // mappings.values.foreach(x => {
  //   println(x)
  //   println("BUM")
  //   println(x.map(_.map(seed)))
  //   var acc = x.foldLeft(seed)((a, b) => b.map(a))
  //   println(acc)
  //   seed = acc
  // })

  println("foo")
  println(mappings.values.map(_.map(_.map2(seed))))
  println(mappings.values.map(_.map(_.map2(seed))))
  println(mappings.values.map(applyMappings(seed, _)))
  println(applyMappings(79, mappings.values.toList(0)))
