package dev.gsoto.pukeko

trait Half {
  def connectsWith(half: Half): Boolean
  def toString: String
}

case class Piece(id: Int, up: Half, right: Half, down: Half, left: Half) {
  lazy val rotations: List[Piece] =
    List(
      this,
      Piece(id, right, down, left, up),
      Piece(id, down, left, up, right),
      Piece(id, left, up, right, down)
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

object Piece {
  val widthChars = 9
  val heightChars = 5
}
