package collapse.strategy

import collapse.Board


class PriorityGroupStrategy(priority: Strategy*) extends Strategy {

  val lazyPrior = priority.view

  override def act(board: Board): Option[Move] = lazyPrior.flatMap(p => p.act(board)).headOption

  def actt(board: Board): Move = act(board).get

}
