package collapse.strategy

import collapse.Board
import collapse.strategy.Direction._


class RandomActionStrategy extends Strategy {

  /**
   * @param from incl
   * @param to excl
   * @return
   */
  def rand(from: Int, to: Int) = (math.random * (to - from)).toInt + from

  override def act(board: Board) = Move(
    from = (rand(1, board.size), rand(1, board.size)),
    direction = Directions(rand(0, Directions.size))
  )
}
