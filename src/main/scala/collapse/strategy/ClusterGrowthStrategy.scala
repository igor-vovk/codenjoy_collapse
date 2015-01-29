package collapse.strategy

import collapse.Board

import scala.collection.immutable.TreeMap
import BoardOps._

object ClusterGrowthStrategy extends Strategy {

  type OrderBy = (Int, Int)

  override def act(board: Board) = {
    val movesByScores = possibleMoves(board)

//    println("All possible moves: " + movesByScores)

    movesByScores.headOption.map(_._2)
  }

  def possibleMoves(board: Board): Map[OrderBy, Move] = {
    val curScore = ClustersDetector.boardScore(board)
    val curEmpty = emptiness(board)
    val moves = MovesGenerator.allPossibleMoves(board)

    (TreeMap.empty[OrderBy, Move](implicitly[Ordering[OrderBy]].reverse) /: moves) {
      case (mem, move) =>
        val scoreDiff = ClustersDetector.boardScore(swap(board, move)) - curScore
        val boardAfterMove = mkMove(board, move)
        val emptyDiff = emptiness(boardAfterMove) - curEmpty

        if (scoreDiff > 0 || emptyDiff > 0) mem + ((scoreDiff, emptyDiff) -> move)
        else mem
    }
  }

}
