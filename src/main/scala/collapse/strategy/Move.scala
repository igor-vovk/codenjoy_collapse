package collapse.strategy

import collapse.Point
import collapse.strategy.Direction._

case class Move(from: Point, direction: Direction) {

  lazy val to: Point = direction.add(from)

}