package collapse.strategy

import collapse.Board

trait Strategy {

  def act(board: Board): Move

}
