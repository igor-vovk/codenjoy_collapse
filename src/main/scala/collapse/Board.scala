package collapse

import collapse.Field.Field

object Field {

  sealed trait Field {
    def ch: Char
  }
  case class Item(ch: Char) extends Field

  val None = Item(' ')
  val Border = Item('☼')

  val NumFields = (0 to 9).map(num => Item(num.toString.apply(0)))
  val NumChars = NumFields.map(field => field.ch -> field).toMap

  def isNumeric(ch: Char): Boolean = NumChars.contains(ch)

  def isNumeric(field: Field): Boolean = NumFields.contains(field)

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

  def contains(point: Point): Boolean

  def apply(point: Point): Option[Field] = if (contains(point)) Some(get(point)) else None

  def get(point: Point): Field

  def updated(point: Point, item: Field): Board

  override def toString: String = "Board(" + Board.serialize(this) + ")"

}

class SeqBackedBoardImpl(underlying: Seq[Seq[Field]]) extends Board {

  override lazy val size = underlying.size

  override def contains(point: Point): Boolean = {
    def isInBounds(a: Int) = a > 0 && a < size

    val (x, y) = point

    Seq(x, y).forall(isInBounds)
  }

  override def get(coords: Point) = underlying(coords._1)(coords._2)

  override def updated(coords: Point, item: Field): Board = {
    require(contains(coords))

    val (x, y) = coords
    new SeqBackedBoardImpl(underlying.updated(x, underlying(x).updated(y, item)))
  }

}
