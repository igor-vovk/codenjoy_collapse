package collapse.strategy

import collapse.Point

object Direction {

  abstract class Direction(xm: Int, ym: Int) {
    def add(base: Point): Point = (base._1 + xm, base._2 + ym)
  }

  case object Up extends Direction(0, -1)

  case object Down extends Direction(0, 1)

  case object Left extends Direction(-1, 0)

  case object Right extends Direction(1, 0)

  val Directions = Seq(Up, Down, Left, Right)

  def strRepr(dir: Direction): String = dir match {
    case Up => "UP"
    case Down => "DOWN"
    case Left => "LEFT"
    case Right => "RIGHT"
  }

}
