package collapse.strategy

import collapse.{Field, Point, Board}

object MovesGenerator {

  private def isNumericField(board: Board, point: Point): Boolean = Field.isNumeric(board.get(point).ch)

  def possibleMovesFromPoint(board: Board, point: Point): Stream[Move] = for {
    dir <- Direction.Directions.toStream
    if {
      val newPoint = dir.add(point)

      board.contains(newPoint) && isNumericField(board, point) && isNumericField(board, newPoint)
    }
  } yield MoveImpl(point, dir)

  def allPossibleMoves(board: Board): Stream[Move] = {
    val coordsStream = Stream.from(1).takeWhile(_ < board.size)

    for {
      x <- coordsStream; y <- coordsStream
      move <- possibleMovesFromPoint(board, (x, y))
    } yield move
  }

}
