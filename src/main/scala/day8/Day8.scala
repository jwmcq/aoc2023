package day8

import scala.io.Source

case class Node(name: String, left: String, right: String)

enum Direction:
  case Left, Right

// val testLines = List(
//   "LLR",
//   "",
//   "AAA = (BBB, BBB)",
//   "BBB = (AAA, ZZZ)",
//   "ZZZ = (ZZZ, ZZZ)"
// )

val testLines = List(
  "LR",
  "",
  "IIA = (IIB, XXX)",
  "IIB = (XXX, IIZ)",
  "IIZ = (IIB, XXX)",
  "TTA = (TTB, XXX)",
  "TTB = (TTC, TTC)",
  "TTC = (TTZ, TTZ)",
  "TTZ = (TTB, TTB)",
  "XXX = (XXX, XXX)"
)

object Parser:
  def parseDirections(line: String): Seq[Direction] =
    line.map {
      _ match
        case 'L' => Direction.Left
        case 'R' => Direction.Right
    }

  def parseNode(line: String): (String, Node) =
    val nodeNames = line.filter(_.isLetter).grouped(3).toList
    (nodeNames(0), Node(nodeNames(0), nodeNames(1), nodeNames(2)))

  def parseLines(lines: Seq[String]): (Iterator[Direction], Map[String, Node]) =
    val directions = Iterator.continually(parseDirections(lines.head)).flatten
    (directions, lines.tail.tail.map(parseNode).toMap)

def walkMap(
    start: String,
    directions: Iterator[Direction],
    nodes: Map[String, Node]
): Iterator[Node] =
  directions.scanLeft(nodes(start)) { (node: Node, dir: Direction) =>
    dir match
      case Direction.Left  => nodes(node.left)
      case Direction.Right => nodes(node.right)
  }

def part2(directions: Iterator[Direction], nodes: Map[String, Node]): Int =
  val aNodes = nodes.keys.filter(_(2) == 'A').toList
  val directions2 = directions.flatMap(List.fill(aNodes.length)(_))
  val walkers = aNodes.map(walkMap(_, directions2, nodes))
  def loop(count: Int, walkers: List[Iterator[Node]]): Int =
    val step = walkers.flatMap(_.nextOption())
    if step.forall(_.name(2) == 'Z') then count
    else loop(count + 1, walkers)

  loop(0, walkers)

@main def day8: Unit =
  // val (directions, nodes) = Parser.parseLines(testLines)
  val (directions, nodes) =
    Parser.parseLines(Source.fromFile("resources/day8.txt").getLines.toList)

  // val steps =
  //   walkMap("AAA", directions, nodes).takeWhile(_.name != "ZZZ").toList
  // println(steps.length)
  println("yarrr")
  println(part2(directions, nodes))
