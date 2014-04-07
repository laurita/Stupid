class Card(s: Suite, r: Char) {

  def this(s: Suite, rnk: String) {
    this(s, rnk.head)
  }

  val rank = r
  val suit = s

  def strength(trump: Suite): Int = {
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
    res.toInt
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
    def rec(left: List[Card], acc: List[Card]): (Option[Card], List[Card]) = {
      left match {
        case Nil => (None, acc.reverse)
        case x::xs if ((this.suit != trump) && (x.suit == this.suit) && (this.strength(trump) < x.strength(trump))) ||
          ((this.suit == trump) && (x.suit == trump) && (this.strength(trump) < x.strength(trump))) ||
          ((this.suit != trump) && (x.suit == trump)) =>
          (Some(x), acc.reverse:::xs)
        case x::xs =>
          rec(xs, x::acc)
      }
    }
    rec(cards, Nil)
  }


}