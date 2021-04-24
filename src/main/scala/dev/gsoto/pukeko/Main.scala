package com.gsoto.pukeko

import com.gsoto.pukeko.PuzzleSolver.Position

object A1 extends Half {
  override val opposite: Half = A2
  override def toString = "A"
}

object A2 extends Half {
  override val opposite: Half = A1
  override def toString = "a"
}

object B1 extends Half {
  override val opposite: Half = B2
  override def toString = "B"
}

object B2 extends Half {
  override val opposite: Half = B1
  override def toString = "b"
}

object C1 extends Half {
  override val opposite: Half = C2
  override def toString = "C"
}

object C2 extends Half {
  override val opposite: Half = C1
  override def toString = "c"
}

object D1 extends Half {
  override val opposite: Half = D2
  override def toString = "D"
}

object D2 extends Half {
  override val opposite: Half = D1
  override def toString = "d"
}

object Z1 extends Half {
  override val opposite: Half = Z2
  override def toString = "X"
}

object Z2 extends Half {
  override val opposite: Half = Z1
  override def toString = "x"
}

object Main {

  val pukekoPieces = List(
    RotatedPiece(1, Z1, A1, B2, C2),
    RotatedPiece(2, Z2, A2, A1, C1),
    RotatedPiece(3, A1, B2, C2, D1),
    RotatedPiece(4, A1, B2, D2, C1),
    RotatedPiece(5, A1, C2, D2, B1),
    RotatedPiece(6, A1, D1, B2, C2),
    RotatedPiece(7, A1, D2, C2, B1),
    RotatedPiece(8, A1, D2, C2, D1),
    RotatedPiece(9, B1, C1, B2, A2),
    RotatedPiece(10, B1, C1, D2, A2),
    RotatedPiece(11, B1, C1, D2, A2), // equal to 10
    RotatedPiece(12, B1, C2, A2, D1),
    RotatedPiece(13, B1, D1, A2, C2),
    RotatedPiece(14, C1, A2, B2, D1),
    RotatedPiece(15, C1, B2, A2, D1),
    RotatedPiece(16, C1, D1, A2, B2)
  )

  val pukekoPositions: List[Position] = List(
    (0, 0), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1), (2, -1), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (-1, 2)

//    (0, 0), (1, 0), (2, 0), (3, 0),
//    (0, 1), (1, 1), (2, 1), (3, 1),
//    (0, 2), (1, 2), (2, 2), (3, 2),
//    (0, 3), (1, 3), (2, 3), (3, 3)
  )

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime
    val solutions: List[List[(Position, RotatedPiece)]] =
      PuzzleSolver.solutions(pukekoPieces, pukekoPositions)
    val duration = (System.nanoTime - t0) / 1e9d

    val nonRotatedSols = solutions.filter { pieceList =>
      val piece01isUp = pieceList.exists {
        case (_, RotatedPiece(1, _, Z1, _, _)) => true
        case _ => false
      }
      val piece10before11 = pieceList.collect {
        case (_, RotatedPiece(id, _, _, _, _)) if id == 10 || id == 11 => id
      } == List(10, 11)
      piece01isUp && piece10before11
    }

    println(s"Number of solutions: ${nonRotatedSols.length}, in $duration seconds.")
    nonRotatedSols.foreach { pieceList =>
      println("---------------------------------------")
      printSolution(pieceList)
    }
  }

  def printSolution(solution: List[(Position, RotatedPiece)]): Unit = {
    val rows = solution.map(_._1._2).distinct
    val cols = solution.map(_._1._1).distinct
    if (rows.nonEmpty) {
      for {
        row <- rows.min to rows.max
        line <- 1 to RotatedPiece.heightChars
      } {
        val rowSolution = solution.filter(_._1._2 == row)
        (cols.min to cols.max).map(col => rowSolution.find(_._1._1 == col)).foreach {
          case Some(((_, _), piece)) => print(piece.toString(line) + " ")
          case None => print(" " * (RotatedPiece.widthChars + 1))
        }
        println()
      }
    }
  }
}
