package collapse.strategy

import collapse.Point
import collapse.strategy.Direction._

trait Move {

  def from: Point

  def direction: Direction

  def to: Point = direction.add(from)

}

case class MoveImpl(from: Point, direction: Direction) extends Move