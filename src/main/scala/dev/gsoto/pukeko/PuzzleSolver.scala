package com.gsoto.pukeko

abstract class Half {
  protected val opposite: Half
  def connectsWith(halve: Half): Boolean = halve == opposite
}

case class RotatedPiece(id: Int, up: Half, right: Half, down: Half, left: Half) {
  lazy val rotations: List[RotatedPiece] =
    List(
      this,
      RotatedPiece(id, right, down, left, up),
      RotatedPiece(id, down, left, up, right),
      RotatedPiece(id, left, up, right, down)
    )

  def toString(row: Int): String = {
    row match {
      case 1 => f"\u250c$id%02d\u2500\u2500\u2500\u2500\u2500\u2510"
      case 2 => s"\u2502   $up   \u2502"
      case 3 => s"\u2502 $left   $right \u2502"
      case 4 => s"\u2502   $down   \u2502"
      case 5 => s"\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518"
    }
  }
}

object RotatedPiece {
  val widthChars = 9
  val heightChars = 5
}

object PuzzleSolver {
  type Position = (Int, Int)

  def solutions(pieces: List[RotatedPiece], positions: List[Position]): List[List[((Int, Int), RotatedPiece)]] = {
    solutions(pieces, positions, Nil)
  }

  private def solutions(pieces: List[RotatedPiece], positions: List[Position], partialSolution: List[(Position, RotatedPiece)]): List[List[(Position, RotatedPiece)]] = {
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
   * Does piece fit into position for partialSolution?
   */
  private def fits(piece: RotatedPiece, position: Position, partialSolution: List[(Position, RotatedPiece)]): Boolean = {
    val up = (position._1, position._2 - 1)
    val right = (position._1 + 1, position._2)
    val down = (position._1, position._2 + 1)
    val left = (position._1 - 1, position._2)
    val matches: List[Boolean] = partialSolution.collect {
      case (pos, upPiece) if pos == up => upPiece.down.connectsWith(piece.up)
      case (pos, rightPiece) if pos == right => rightPiece.left.connectsWith(piece.right)
      case (pos, downPiece) if pos == down => downPiece.up.connectsWith(piece.down)
      case (pos, leftPiece) if pos == left => leftPiece.right.connectsWith(piece.left)
    }
    // All matches are true.
    matches.forall(identity)
  }
}
