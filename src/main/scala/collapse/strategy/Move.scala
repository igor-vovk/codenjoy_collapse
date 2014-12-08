package collapse.strategy

import collapse.Point
import collapse.strategy.Direction._

trait Move {

  def coords: Point

  def direction: Direction

}

case class MoveImpl(coords: Point, direction: Direction) extends Move