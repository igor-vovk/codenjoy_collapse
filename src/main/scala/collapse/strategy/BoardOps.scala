package collapse.strategy

import collapse.Field.Field
import collapse._

object BoardOps {

  def fields(board: Board): Stream[(Point, Field)] = {
    val coords = Stream.from(0).takeWhile(_ < board.size)

    for (x <- coords; y <- coords) yield {
      val point = (x, y)

      (point, board.get(point))
    }
  }

  def numericFields(board: Board): Stream[(Point, Field)] = fields(board).filter {
    case (_, field) => Field.isNumeric(field)
  }

  def mkMove(board: Board, move: Move): Board = ???

}
