/**
 * ## Stupid ##
 *
 * There's a Lithuanian/Russian card game called 'Stupid'.
 *
 * It has these (simplified) rules:
 *
 * Suites: Hearts, Diamonds, Clubs, Spades.
 * Card ranks: 2, 3, 4, 5, 6, 7, 8, 9, T, Jack, Queen, King, Ace.
 *
 * Cards are ordered by their rank and then their suite.
 *
 * One suite is called the trump suite. Any trump card is always higher than
 * any non-trump card.
 *
 * The trump suite is represented as the first line in data file by a letter:
 * H|D|C|S.
 *
 * Two players get their cards which are represented by a line in the file:
 * ST D2 CJ HQ DA | H2 D3 C4 S5 H6 D7 C8 S9
 *
 * First player starts as offense. Offense always starts.
 *
 * 1) He plays smallest non-trump card. If he does not have non-trump card,
 * he plays smallest trump card then.
 * 2) If second player has cards of the same rank, he plays smallest of them
 * and passes the turn to P1. Roles of the attacker/defender switch then.
 * 3) Whenever there is no card to be passed, defender must use
 * smallest non-trump cards that are bigger than the each card on the table
 * to cover them, starting with the smallest uncovered cards currently on
 * table. If he doesn't have a non-trump card to cover any card, he
 * can use a trump card to do that. If he doesn't have any card to cover current
 * card, he shouldn't play any more cards in this turn.
 * 4) Attacker then can add more cards that are of the same rank that
 * currently exist on the table for defender to current. Maximum number of
 * non-covered cards on the table cannot exceed defenders current hand size.
 * If such scenario arises, attacker then plays the smallest card from their
 * hand.
 * 5a) If all cards are covered - defender wins this round, cards are
 * discarded and defense becomes offense for the next turn.
 * 5b) If not all cards are covered - go to 3). If defender can't cover all cards
 * it loses the round and takes all cards to his hand. It's offense turn.
 *
 * Repeat this until one player does not have cards in his hand after a turn.
 * That player wins.
 *
 * For each given line, print 1 if p1 wins, 2 if p2 wins.
 */
class Stupid(tr: Suite, offense: Player, defense: Player) {
  val TableMax = 6
  val trump = tr
  val t: List[List[Card]] = List()

  // sort players' cards baring trump suite in mind
  val o = new Player(offense.sortCards(trump), offense.number)
  val d = new Player(defense.sortCards(trump), defense.number)

  def play(): Int = play(o, d, Nil, Nil, 1)

  def play(o: Player, d: Player, toDefend: List[Card], defended: List[Card], state: Int): Int = {
    println(s"o: $o, d: $d, toDefend: $toDefend, defended: $defended, state: $state")
    state match {
      // passing state
      case 1 =>
        println("state 1")
        if (o.cards.isEmpty) o.number
        else {
          // first card on table
          if (toDefend.isEmpty) {
            println("first card")
            play(d, new Player(o.cards.tail, o.number), Player.sortCards(o.cards.head::toDefend, trump), Nil, state)
          }
          // other passes
          else {
            if (d.cards.length <= toDefend.length) {
              play(d, o, toDefend, defended, 2)
            } else {
              println("other passes?")
              val passRank = toDefend.head.rank
              val (removed, remaining) = o.removeFirstOfRank(passRank)
              removed match {
                // can pass
                case Some(x) =>
                  println(s"passing with $x")
                  play(d, new Player(remaining, o.number), Player.sortCards(x::toDefend, trump), Nil, state)
                // no card to pass, go to defending state
                case None =>
                  println("cannot pass")
                  play(d, o, toDefend, Nil, 2)
              }
            }

          }
        }
      // defending state, defender starts defending
      case 2 =>
        println("state 2")
        if (d.cards.isEmpty) d.number
        else {
          toDefend match {
            // nothing to defend
            case Nil =>
              println("nothing to defend")
              val (toAdd, atHand) = o.addCards(defended, d.cards.length, trump)
              // attacker has nothing to add
              if (toAdd.isEmpty) {
                println("attacker cannot add any cards")
                play(d, o, Nil, Nil, 1)
              }
              // attacker adds cards to defend
              else {
                println(s"attacker adds $toAdd")
                val pl = new Player(atHand, o.number)
                play(pl, d, Player.sortCards(toAdd, trump), defended, 2)
              }

            // cards left to defend, defender defends
            case c::cs =>
              println(s"cards left to defend: $toDefend")
              val (defender, remaining) = c.defenderAndRemaining(d.cards, tr)
              defender match {
                // defender cannot defend, go to adding state before taking cards
                case None =>
                  println(s"defender cannot defend against $c")
                  play(o, d, toDefend, defended, 3)
                // can defend, defends, go back to stage 2 to defend remaining cards
                case Some(x) =>
                  println(s"defender defends against $c with $defender")
                  play(o, new Player(remaining, d.number), cs, x::c::defended, 2)
              }
          }
        }

      // attacker adds cards before defender takes home
      case 3 =>
        println("state 3")
        // according to specs, I attacker cannot add more cards than defender has at hand
        val (toAdd, atHand) = o.addCards(defended:::toDefend, d.cards.length - toDefend.length, trump)
        println(s"attacker adds $toAdd and defender takes home")
        val pl1 = new Player(atHand, o.number)
        val pl2 = new Player(Player.sortCards(d.cards ::: toAdd ::: defended ::: toDefend, trump), d.number)
        play(pl1, pl2, Nil, Nil, 1)
    }
  }

}
