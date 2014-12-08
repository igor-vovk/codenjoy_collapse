package collapse.strategy

import collapse.{Field, Point, Board}

object MovesGenerator {

  private def isNumericField(board: Board, point: Point) = board(point).map(_.ch).exists(Field.isNumeric)

  def possibleMovesFromPoint(board: Board, point: Point): Stream[Move] = for {
    dir <- Direction.Directions.toStream
    if isNumericField(board, point) && isNumericField(board, dir.add(point))
  } yield MoveImpl(point, dir)

  def allPossibleMoves(board: Board): Stream[Move] = {
    val coordsStream = Stream.from(1).takeWhile(_ < board.size)

    for {
      x <- coordsStream; y <- coordsStream
      move <- possibleMovesFromPoint(board, (x, y))
    } yield move
  }

}
