package collapse.strategy

import collapse.strategy.Direction._

trait Move {

  def coords: (Int, Int)

  def direction: Direction

}
