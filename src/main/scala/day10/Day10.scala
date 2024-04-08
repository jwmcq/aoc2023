package day10

case class Coord(x: Int, y: Int):
  // format: off
  val neighbourOffsets = List(
    (-1, -1), ( 0, -1), ( 1, -1),
    (-1,  0), ( 0,  0), ( 1,  0),
    (-1,  1), ( 0,  1), ( 1,  1)
  )
  // format: on

  def tuple = (x, y)

  def +(that: Coord) = Coord(this.x + that.x, this.y + that.y)
  def +(tpl: (Int, Int)) = Coord(this.x + tpl._1, this.y + tpl._2)

  def neigbours = neighbourOffsets.map(this + _)

sealed trait Tile(symbol: Char):
  def openings: List[Coord] = List()

enum Pipe(symbol: Char, inlets: List[Coord]) extends Tile(symbol):
  override def openings = this.inlets
  case `|` extends Pipe('|', List(Coord(0, -1), Coord(0, 1)))
  case `-` extends Pipe('-', List(Coord(-1, 0), Coord(1, 0)))
  case `L` extends Pipe('L', List(Coord(0, -1), Coord(1, 0)))
  case `J` extends Pipe('J', List(Coord(0, -1), Coord(-1, 0)))
  case `7` extends Pipe('7', List(Coord(-1, 0), Coord(0, 1)))
  case `F` extends Pipe('F', List(Coord(0, -1), Coord(1, 0)))

case object Ground extends Tile('.')
case object Start extends Tile('S')

case class MapTile(symbol: Char, loc: Coord) extends Tile(symbol)

type TileMap = Array[Array[MapTile]]

extension (map: TileMap) def loc(x: Int, y: Int): MapTile = map(y)(x)

object Parser:
  def parseLines(lines: Seq[String]): TileMap =
    // seq.map(_.map())
    ???

@main def day10: Unit =
  val foo = Pipe.valueOf("|").openings
  println(foo)
  println("foo")
