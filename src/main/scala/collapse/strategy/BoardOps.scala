package collapse.strategy

import collapse.Field.{Empty, Field}
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

  // Swap two points
  def swap(board: Board, a: Point, b: Point): Board = {
    require(math.abs(a._1 - b._1) + math.abs(a._2 - b._2) == 1, "Distance between points must equal to 1")
    val oldA = board.get(a)
    val oldB = board.get(b)

    require(
      Field.isNumeric(oldA) && Field.isNumeric(oldB),
      s"Can swap only numeric fields (trying to swap fields $a -> $oldA and $b -> $oldB)"
    )

    board.updated(a, oldB).updated(b, oldA)
  }

  def swap(board: Board, move: Move): Board = swap(board, move.from, move.to)

  def shift(fields: List[Field]): List[Field] = fields match {
    case Nil => Nil
    // If current element is empty,
    // find first non-empty element and shift it to the current position,
    // replacing by empty element and repeating shifting procedure starting from new current element
    case Field.Empty :: tail   =>
      tail.find(Field.isNumeric) match {
        case Some(numericField) => shift(numericField :: tail.updated(tail.indexOf(numericField), Empty))
        case None => fields // Empty elements everywhere, no need to check next
      }
    case head :: tail => head :: shift(tail)
  }

  def mkMove(board: Board, move: Move): Board = {
    // 1. Just swap two points
    val b1 = swap(board, move)

    // 2. Remove clusters, with size gt 1, in which swapping points belongs to
    val b2 = {
      val pointsToRemove = ClustersDetector.detect(b1)
        .filter(cluster => cluster.size > 1 && (cluster(move.from) || cluster(move.to)))
        .flatten

      (b1 /: pointsToRemove) {
        case (m, p) => m.updated(p, Field.Empty)
      }
    }

    // 3. Fall down points, located under empty places
    val b3 = (b2 /: (0 to (b2.size - 1))) {
      case (b, x) => VerticalProjection.updated(b, x, shift(VerticalProjection.get(b, x).reverse).reverse)
    }

    b3
  }

}
