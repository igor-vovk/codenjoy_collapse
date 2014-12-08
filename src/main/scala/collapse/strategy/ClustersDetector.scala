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

  /**
   * Relation between cluster size and score:
   * 1 => 1
   * 2 => 1 + 2 = 3
   * 3 => 1 + 2 + 3 = 6
   * @param cluster
   * @return
   */
  def score(cluster: Cluster): Int = (1 to cluster.size).sum

  /**
   * Overall score of board clusters
   * @param board
   * @return
   */
  def boardScore(board: Board): Int = detect(board).map(score).sum
}
