package collapse.strategy

import collapse.strategy.Direction.Direction
import collapse.{Field, Point, Board}

object MovesGenerator {

  def possibleMovesFromPoint(board: Board, point: Point): Stream[Move] = {
    def isMoveAllowed(dir: Direction): Boolean =
      (board(point), board(dir.add(point))) match {
        // Each field found or not
        case (Some(fromField), Some(toField)) =>
          // Fields must not have same values (move between "1" and "1" are not allowed) and be numeric
          fromField != toField && Field.isNumeric(fromField) && Field.isNumeric(toField)
        case _ => false
      }

    for {
      dir <- Direction.Directions.toStream
      if isMoveAllowed(dir)
    } yield MoveImpl(point, dir)
  }

  def allPossibleMoves(board: Board): Stream[Move] = BoardOps.fields(board).flatMap {
    case (point, _) => possibleMovesFromPoint(board, point)
  }

}
