package collapse.strategy

import collapse.Board


object BestScoreStrategy extends Strategy {

  override def act(board: Board) = {
    val curScore = ClustersDetector.boardScore(board)
    val moves = MovesGenerator.allPossibleMoves(board)

    val movesByScores = moves
      .flatMap(move => {
        val newScore = ClustersDetector.boardScore(BoardOps.swap(board, move))

        if (newScore > curScore) Seq((newScore * -1, move))
        else Seq.empty
      })
      .sortBy(_._1)

//    println("All possible moves: " + movesByScores.toMap)

    movesByScores.headOption.map(_._2)
  }
}
