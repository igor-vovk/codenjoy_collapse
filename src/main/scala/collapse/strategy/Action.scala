package collapse.strategy

import collapse.strategy.Direction._

trait Action {

  def coords: (Int, Int)

  def direction: Direction

}
