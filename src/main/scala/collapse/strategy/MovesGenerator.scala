package collapse.strategy

import collapse.strategy.Direction.Direction
import collapse.{Field, Point, Board}

object MovesGenerator {

  /**
   * At depth 1
   * @param board
   * @param point
   * @return
   */
  def possibleMovesFromPoint(board: Board, point: Point,
                             ignoreSameFields: Boolean = true): Stream[Move] = {
    def isMoveAllowed(dir: Direction): Boolean =
      (board(point), board(dir.add(point))) match {
        // Each field found or not
        case (Some(fromField), Some(toField)) =>
          // Fields must not have same values (move between "1" and "1" are not allowed) and be numeric
          ((ignoreSameFields && fromField != toField) || !ignoreSameFields) && Field.isNumeric(fromField) && Field.isNumeric(toField)
        case _ => false
      }

    Direction.Directions
      .toStream
      .filter(isMoveAllowed)
      .map(dir => Move(point, dir))
  }

  def allPossibleMoves(board: Board): Stream[Move] = BoardOps.fields(board).flatMap {
    case (point, _) => possibleMovesFromPoint(board, point)
  }

}
