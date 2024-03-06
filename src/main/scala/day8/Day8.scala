package day8

import scala.io.Source

case class Node(name: String, left: String, right: String)

enum Direction:
  case Left, Right

val testLines = List(
  "LLR",
  "",
  "AAA = (BBB, BBB)",
  "BBB = (AAA, ZZZ)",
  "ZZZ = (ZZZ, ZZZ)"
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

  def parseLines(lines: Seq[String]): (LazyList[Direction], Map[String, Node]) =
    val directions = LazyList.continually(parseDirections(lines.head)).flatten
    (directions, lines.tail.tail.map(parseNode).toMap)

def walkMap(
    start: String,
    directions: LazyList[Direction],
    nodes: Map[String, Node]
): LazyList[Node] =
  directions.scanLeft(nodes(start)) { (node: Node, dir: Direction) =>
    dir match
      case Direction.Left  => nodes(node.left)
      case Direction.Right => nodes(node.right)
  }

def pt2(directions: LazyList[Direction], nodes: Map[String, Node]): Int =
  val aNodes = nodes.keys.filter(_(2) == 'A').toList
  println(aNodes)
  val walkers = aNodes.map(walkMap(_, directions, nodes).toIterator)
  println(walkers.toList)
  val paths = walkers
    .takeWhile((ws: Iterator[Node]) => !(ws.forall(_.name(2) == 'Z')))
    .toList
    .head
    .length
  println(paths)
  1

def part2(directions: LazyList[Direction], nodes: Map[String, Node]): Int =
  val aNodes = nodes.keys.filter(_(2) == 'A').toList
  for {
    walkers <- aNodes.map(walkMap(_, directions, nodes).toIterator)
  }
    1

@main def day8: Unit =
  // val (directions, nodes) = Parser.parseLines(testLines)
  val (directions, nodes) =
    Parser.parseLines(Source.fromFile("resources/day8.txt").getLines.toList)

  val steps =
    walkMap("AAA", directions, nodes).takeWhile(_.name != "ZZZ").toList
  // println(steps.length)
  println(pt2(directions, nodes))
