package collapse.strategy

object Direction {

  sealed trait Direction

  case object Up extends Direction

  case object Down extends Direction

  case object Left extends Direction

  case object Right extends Direction

  val Directions = Seq(Up, Down, Left, Right)

  def strRepr(dir: Direction): String = dir match {
    case Up => "UP"
    case Down => "DOWN"
    case Left => "LEFT"
    case Right => "RIGHT"
  }

}
