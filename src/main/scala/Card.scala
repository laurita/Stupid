
object Card {

}

class Card(s: Suite, r: Char) {

  def this(s: Suite, rnk: String) {
    this(s, rnk.head)
  }

  val rank = r
  val suit = s
  val strength = r match {
    case x if x >= 50 && x <= 57 =>
      x.toInt
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
  }

  override def toString: String = {
    rank.toString
  }

  override def equals(other: Any): Boolean =
    other match {
      case o: Card =>
        (o canEqual this) &&
          rank == o.rank &&
          suit == o.suit

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Card]

  override def hashCode: Int = {
    (strength.hashCode() * Math.pow(31, 2) + suit.hashCode() * Math.pow(31, 1)).toInt
  }


}