package dev.gsoto.pukeko

import dev.gsoto.pukeko.PuzzleSolver.Position

object Main {

  // opposite should be passed by name because of object circular references below.
  sealed abstract class PukekoHalf(opposite: => PukekoHalf, text: String) extends Half {
    override def connectsWith(half: Half): Boolean = half == opposite
    override def toString: String = text
  }

  // All types of halves in the puzzle
  // Letter denotes the type of pukeko
  // Uppercase/lowercase denotes head or legs
  object A1 extends PukekoHalf(A2, "A")
  object A2 extends PukekoHalf(A1, "a")
  object B1 extends PukekoHalf(B2, "B")
  object B2 extends PukekoHalf(B1, "b")
  object C1 extends PukekoHalf(C2, "C")
  object C2 extends PukekoHalf(C1, "c")
  object D1 extends PukekoHalf(D2, "D")
  object D2 extends PukekoHalf(D1, "d")
  object Z1 extends PukekoHalf(Z2, "Z")
  object Z2 extends PukekoHalf(Z1, "z")

  // All pieces in the puzzle
  val pukekoPieces = List(
    Piece(1, Z1, A1, B2, D2),
    Piece(2, Z2, A2, A1, D1),
    Piece(3, A1, B2, D2, C1),
    Piece(4, A1, B2, C2, D1),
    Piece(5, A1, D2, C2, B1),
    Piece(6, A1, C1, B2, D2),
    Piece(7, A1, C2, D2, B1),
    Piece(8, A1, C2, D2, C1),
    Piece(9, B1, D1, B2, A2),
    Piece(10, B1, D1, C2, A2),
    Piece(11, B1, D1, C2, A2), // identical to piece 10
    Piece(12, B1, D2, A2, C1),
    Piece(13, B1, C1, A2, D2),
    Piece(14, D1, A2, B2, C1),
    Piece(15, D1, B2, A2, C1),
    Piece(16, D1, C1, A2, B2)
  )

  // Sequence of positions to fulfill
  val pukekoPositions: List[Position] = List(
    // 4x4 grid, spiraling from the center
    (0, 0), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1), (2, -1), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (-1, 2)
  )

  def main(args: Array[String]): Unit = {
    // Find solutions
    val startTime = System.nanoTime
    val solutions: List[List[(Position, Piece)]] = PuzzleSolver.solutions(pukekoPieces, pukekoPositions)
    val duration: Double = (System.nanoTime - startTime) / 1e9d

    // Filter duplicate solutions (rotations and  identical pieces)
    val filteredSolutions = solutions.filter { pieceList =>
      val isPiece1up = pieceList.exists {
        case (_, Piece(1, _, Z1, _, _)) => true
        case _ => false
      }
      val isPiece10before11 = pieceList.collect {
        case (_, Piece(id, _, _, _, _)) if id == 10 || id == 11 => id
      } == List(10, 11)
      isPiece1up && isPiece10before11
    }

    // Print results
    println(s"${solutions.length} solutions found in $duration seconds")
    println(s"${filteredSolutions.length} after removing rotations and duplicates")
    filteredSolutions.zip(LazyList.from(1)).foreach { case (solution, index) =>
      println()
      println(s"Solution $index")
      printSolution(solution)
    }
  }

  def printSolution(solution: List[(Position, Piece)]): Unit = {
    val presentRows: List[Int] = solution.map { case ((_, y), _) => y }.distinct
    val presentCols: List[Int] = solution.map { case ((x, _), _) => x }.distinct
    if (presentRows.nonEmpty) {
      for {
        row <- presentRows.min to presentRows.max
        line <- 1 to Piece.heightChars
      } {
        val rowSolution = solution.filter(_._1._2 == row)
        (presentCols.min to presentCols.max).map(col => rowSolution.find(_._1._1 == col)).foreach {
          case Some(((_, _), piece)) => print(piece.toString(line) + " ")
          case None => print(" " * (Piece.widthChars + 1))
        }
        println()
      }
    }
  }
}
