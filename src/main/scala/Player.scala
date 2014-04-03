object Player {
  def sortCards(cs: List[Card], trump: Suite): List[Card] = {
    val res = cs.sortBy(_.strength(trump))
    res
  }
}

class Player(cs: List[Card], n: Int) {
  val cards = cs
  val number = n

  override def toString = {
    "Player(%s, %d)".format(cs.toString(), this.number)
  }

  def smallestNonTrump(trump: Suite): Card = {
    val nonTrump: List[Card] = cards.filterNot( c => c.suit.equals(trump)).sortBy(_.hashCode)
    println(nonTrump)
    nonTrump.head
  }



  def sortCards(trump: Suite): List[Card] = {
    this.cards.sortBy(_.strength(trump))
  }

  def removeFirstOfRank(rank: Char): (Option[Card], List[Card]) = {
    val (before, atAndAfter) = cards span { x => !(x.rank == rank) }
    if (atAndAfter.isEmpty) {
      (None, before)
    } else {
      (Option(atAndAfter.head), before ::: atAndAfter.tail)
    }
  }

  def getDefenceCards(toBeDefended: List[Card], trump: Suite): (List[Card], List[Card]) = {
    //println("toBeDefended: "+ toBeDefended)
    def rec(toDefend: List[Card], defence: List[Card], remaining: List[Card]): (List[Card], List[Card]) = {
      //println("toDefend: "+ toDefend + " defence: "+ defence + " remaining: "+ remaining)
      toDefend match {
        case Nil => (defence.reverse, remaining.reverse)
        case x::xs =>
          val (defender, rem) = x.defenderAndRemaining(remaining, trump)
          defender match {
            case None =>
              (Nil, rem)
            case Some(c) => rec(xs, c::defence, rem)
          }
      }
    }
    rec(toBeDefended, Nil, this.cards)
  }

  // returns cards to add to table, cards left at hand
  def addCards(onTable: List[Card], maxToAdd: Int, trump: Suite): (List[Card], List[Card]) = {
    val (toAdd, notToAdd) = this.cards.partition{ pc =>
      onTable.exists( tc => tc.rank == pc.rank)
    }
    if (toAdd.length > maxToAdd) {
      val (add, leave) = toAdd.splitAt(maxToAdd)
      (add, Player.sortCards(notToAdd:::leave, trump))
    } else {
      (toAdd, notToAdd)
    }
  }
}
