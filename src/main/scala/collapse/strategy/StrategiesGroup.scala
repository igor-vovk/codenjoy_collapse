package collapse.strategy

import collapse.Board


class StrategiesGroup(priority: Strategy*) extends Strategy {

  val strategies = priority.toList

  override def act(board: Board): Option[Move] = {
    def doAct(strategies: List[Strategy]): Option[Move] = strategies match {
      case Nil => None
      case strategy :: tail => strategy.act(board).orElse(doAct(tail))
    }

    doAct(strategies)
  }

  def actt(board: Board): Move = act(board).get

}
