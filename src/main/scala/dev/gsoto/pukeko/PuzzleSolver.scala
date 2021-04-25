package dev.gsoto.pukeko

object PuzzleSolver {

  /**
   * A position (column, row) in the board where a piece should be placed.
   * column number goes up to the right
   * row number goes up
   */
  type Position = (Int, Int)

  /**
   * Finds all solutions for placing a list of pieces into a list of positions, ensuring their halves match
   * @return The list of solutions, each one as a list of piece-position pairs
   */
  def solutions(pieces: List[Piece], positions: List[Position]): List[List[(Position, Piece)]] = {
    solutions(pieces, positions, Nil)
  }

  private def solutions(pieces: List[Piece], positions: List[Position], partialSolution: List[(Position, Piece)]): List[List[(Position, Piece)]] = {
    if (positions.isEmpty) {
      List(partialSolution)
    }
    else {
      val nextPosition = positions.head
      pieces.flatMap { piece =>
        piece.rotations
          .withFilter(rotation => fits(rotation, nextPosition, partialSolution))
          .flatMap { rotation =>
            val remainingPieces = pieces.filterNot(piece.equals)
            val remainingPositions = positions.tail
            val expandedPartialSolution = partialSolution :+ (nextPosition, rotation)
            solutions(remainingPieces, remainingPositions, expandedPartialSolution)
          }
      }
    }
  }

  /**
   * Does piece fit into position for a given partialSolution?
   */
  private def fits(piece: Piece, position: Position, partialSolution: List[(Position, Piece)]): Boolean = {
    val (x, y) = position
    val up = (x, y - 1)
    val right = (x + 1, y)
    val down = (x, y + 1)
    val left = (x - 1, y)
    val matches: List[Boolean] = partialSolution.collect {
      case (`up`, upPiece) => upPiece.down.connectsWith(piece.up)
      case (`right`, rightPiece) => rightPiece.left.connectsWith(piece.right)
      case (`down`, downPiece) => downPiece.up.connectsWith(piece.down)
      case (`left`, leftPiece) => leftPiece.right.connectsWith(piece.left)
    }
    // All matches must be true.
    matches.forall(identity)
  }
}
