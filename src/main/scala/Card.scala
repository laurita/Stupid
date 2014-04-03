
object Card {

}

class Card(s: Suite, r: Char) {

  def this(s: Suite, rnk: String) {
    this(s, rnk.head)
  }

  val rank = r
  val suit = s

  def strength(trump: Suite) = {
    //println("trump: "+ trump)
    val res = if (suit == trump)
      Math.pow(20, 2) + suit.strength
    else
      r match {
        case x if x >= 50 && x <= 57 =>
          Math.pow(x.asDigit, 2) + suit.strength
        case 'T' => Math.pow(10, 2) + suit.strength
        case 'J' => Math.pow(11, 2) + suit.strength
        case 'Q' => Math.pow(12, 2) + suit.strength
        case 'K' => Math.pow(13, 2) + suit.strength
        case 'A' => Math.pow(14, 2) + suit.strength

      }
    //println(this + " strength is " + res)
    res
  }

  override def toString: String = {
    suit.toString + rank.toString
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

  //override def hashCode: Int = {
  //  (strength.hashCode() * Math.pow(31, 2) + suit.hashCode() * Math.pow(31, 1)).toInt
  //}

  //def trumpHashCode: Int = {
  //  (20.hashCode() * Math.pow(31, 3)).toInt
  //}

  // returns (defender card, cards except defender)
  def defenderAndRemaining(cards: List[Card], trump: Suite): (Option[Card], List[Card]) = {
    println("cards: "+ cards)
    def rec(left: List[Card], acc: List[Card]): (Option[Card], List[Card]) = {
      println("left: "+ left +", acc: "+ acc)
      left match {
        case Nil => (None, acc.reverse)
        case x::xs if ((this.suit != trump) && (x.suit == this.suit) && (x.rank > this.rank)) ||
          ((this.suit == trump) && (x.suit == trump) && (x.rank > this.rank)) ||
          ((this.suit != trump) && (x.suit == trump)) =>
          (Some(x), acc.reverse:::xs)
        case x::xs =>
          rec(xs, x::acc)
      }
    }
    val res = rec(cards, Nil)
    println("defender of "+ this +" is "+ res._1 +" remaining: "+ res._2)
    res
  }


}