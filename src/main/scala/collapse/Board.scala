package collapse

object BoardItem {

  sealed trait BoardItem

  case class Item(ch: Char) extends BoardItem

  case class NumericItem(ch: Char, number: Int) extends BoardItem

  val None = Item(' ')
  val Border = Item('☼')
  val NumChars = (0 to 9).map { num =>
    val chRepr = num.toString.apply(0)

    chRepr -> NumericItem(chRepr, num)
  }.toMap
  println(NumChars)

  def charToBoardItem(ch: Char): Option[BoardItem] = ch match {
    case None.ch => Some(None)
    case Border.ch => Some(Border)
    case _ => NumChars.get(ch)
  }

}

object Board {

  def parse(str: String): Board = {
    val length = math.sqrt(str.length).toInt

    val boardArr = for {
      i <- 0 to math.pow(length - 1, 2).toInt by length
    } yield str.substring(i, i + length).toArray.flatMap(BoardItem.charToBoardItem).toSeq

    new Board(boardArr)
  }

}

case class Board(underlying: Seq[Seq[BoardItem.BoardItem]]) {

}
