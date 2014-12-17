package collapse

import collapse.Field.Field

object Field {

  sealed trait Field {
    def ch: Char
  }

  case object Empty extends Field {
    val ch = ' '
  }

  case object Border extends Field {
    val ch = '☼'
  }

  case class Item(ch: Char) extends Field

  val NumFields = (0 to 9).map(num => Item(num.toString.apply(0)))
  val NumChars = NumFields.map(field => field.ch -> field).toMap

  def isNumeric(ch: Char): Boolean = NumChars.contains(ch)

  def isNumeric(field: Field): Boolean = NumFields.contains(field)

  def charToField(ch: Char): Option[Field] = ch match {
    case Empty.ch => Some(Empty)
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

  //  def generate(size: Int): Board = {
  //    val range = 0 to (size - 1)
  //
  //
  //    val a = for {
  //      y <- 0 to (size - 1)
  //      x <- 0 to (size - 1)
  //    } yield Field.NumFields((math.random * 9).toInt)
  //
  //    new SeqBackedBoardImpl(a)
  //  }

  def serialize(board: Board): String = {
    val sb = new StringBuilder
    sb.append("\r\n")

    for {
      y <- 0 to (board.size - 1)
      x <- 0 to (board.size - 1)
    } {
      sb.append(board.get(x, y).ch)
      if (x == board.size - 1) {
        sb.append("\r\n")
      }
    }

    sb.toString()
  }

}

trait Board {

  def size: Int

  def contains(point: Point): Boolean

  def apply(point: Point): Option[Field] = if (contains(point)) Some(get(point)) else None

  def get(point: Point): Field

  def updated(point: Point, item: Field): Board

  override def toString: String = "Board(" + Board.serialize(this) + ")"

}

class SeqBackedBoardImpl(underlying: Seq[Seq[Field]]) extends Board {

  override lazy val size = underlying.size

  override def contains(point: Point): Boolean = {
    val (x, y) = point

    x >= 0 && x < size && y >= 0 && y < size
  }

  override def get(coords: Point) = underlying(coords._2)(coords._1)

  override def updated(coords: Point, item: Field): Board = {
    require(contains(coords), s"Board doesn't contains point $coords")

    val (x, y) = coords
    new SeqBackedBoardImpl(underlying.updated(y, underlying(y).updated(x, item)))
  }

}

trait Projection {
  def get(board: Board, lineNum: Int): List[Field]

  def updated(board: Board, lineNum: Int, value: List[Field]): Board
}

object VerticalProjection extends Projection {
  override def updated(board: Board, x: Int, value: List[Field]): Board = {
    require(value.size == board.size, "Row length must be equal to board size")

    (board /: value.zipWithIndex) {
      case (b, (field, y)) =>
        val coords = (x, y)

        if (b.get(coords) != field) b.updated(coords, field) else b
    }
  }

  override def get(board: Board, x: Int): List[Field] = (0 to (board.size - 1)).map(y => board.get(x, y)).toList
}