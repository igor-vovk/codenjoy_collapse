package collapse.strategy

import collapse.{Board, Field, Point}

import scala.annotation.tailrec

object ClustersDetector {

  type Cluster = Set[Point]

  @tailrec
  def detect(board: Board, clusters: Seq[Cluster] = Seq.empty): Seq[Cluster] = {
    val fields = BoardOps.numericFields(board)

    fields.headOption match {
      case Some((startPos, fieldType)) =>
        val cluster = (Set(startPos) /: fields.tail.filter(_._2 == fieldType)) {
          case (mem, (point, _)) =>
            if (MovesGenerator.possibleMovesFromPoint(board, point, ignoreSameFields = false).exists(move => mem(move.to)))
              mem + point
            else
              mem
        }

        // Change already detected fields to empty ones
        val newBoard = (board /: cluster) {
          case (b, point) => b.updated(point, Field.None)
        }

        detect(newBoard, clusters :+ cluster)
      case None => clusters
    }
  }

}
