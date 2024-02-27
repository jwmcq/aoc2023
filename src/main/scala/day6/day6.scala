package day6

case class Race(time: Int, distance: BigInt) {
  def run(holdTime: BigInt): BigInt =
    holdTime * (BigInt(time) - holdTime)

  def isWin(holdTime: BigInt): Boolean =
    run(holdTime) > distance

  def winningAttempts: List[Int] =
    (0 to time).filter(isWin(_)).toList
}

val races = List(
  Race(42, 308),
  Race(89, 1170),
  Race(91, 1291),
  Race(89, 1467)
)

@main def day6: Unit =
  println(races.map(_.winningAttempts.length).product)
  println(Race(42899189, 308117012911467L).winningAttempts.length)
