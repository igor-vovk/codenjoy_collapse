package collapse

import collapse.Field.{NumericField, Field}

object Field {

  sealed trait Field {
    def ch: Char
  }

  case class Item(ch: Char) extends Field

  case class NumericField(ch: Char, number: Int) extends Field

  val None = Item(' ')
  val Border = Item('☼')
  val NumChars = (0 to 9).map { num =>
    val chRepr = num.toString.apply(0)

    chRepr -> NumericField(chRepr, num)
  }.toMap

  def charToField(ch: Char): Option[Field] = ch match {
    case None.ch => Some(None)
    case Border.ch => Some(Border)
    case _ => NumChars.get(ch)
  }

}

object Board {

  def parse(str: String): Board = {
    val length = math.sqrt(str.length).toInt

    val boardArr = for {
      i <- 0 to ((length - 1) * length) by length
    } yield str.substring(i, i + length).toArray.flatMap(Field.charToField).toSeq

    new SeqBackedBoardImpl(boardArr)
  }

  def serialize(board: Board): String = {
    val sb = new StringBuilder
    sb.append("\r\n")

    for {
      x <- 0 to (board.size - 1)
      y <- 0 to (board.size - 1)
    } {
      sb.append(board.get(x, y).ch)
      if (y == board.size - 1) {
        sb.append("\r\n")
      }
    }

    sb.toString()
  }

}

trait Board {

  def size: Int

  def get(coords: Coords): Field

  def set(coords: Coords, item: NumericField): Board

  override def toString: String = "Board(" + Board.serialize(this) + ")"

}

class SeqBackedBoardImpl(underlying: Seq[Seq[Field]]) extends Board {

  override def size = underlying.size

  override def get(coords: Coords) = underlying(coords._1)(coords._2)

  override def set(coords: Coords, item: NumericField): Board = {
    val (x, y) = coords

    // Check that coordinate is in bounds
    for (coord <- Seq(x, y)) {
      require(coord > 0 && coord < size)
    }

    new SeqBackedBoardImpl(underlying.updated(x, underlying(x).updated(y, item)))
  }

}
