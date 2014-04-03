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
object Stupid {
}

class Stupid(tr: Suite, offense: Player, defense: Player) {
  val TableMax = 6
  val trump = tr
  val t: List[List[Card]] = List()

  // sort players' cards baring trump suite in mind
  val o = new Player(offense.sortCards(trump), offense.number)
  val d = new Player(defense.sortCards(trump), defense.number)

  def play(): Int = play(o, d, tr, Nil, Nil, 1)

  // 1 - passing state, 2 - defending state, 3 - adding cards to defend state
  def play(o: Player, d: Player, tr: Suite, toDefend: List[Card], defended: List[Card], state: Int): Int = {
    //println(s"o: $o, d: $d, trump: $trump, toDefend: $toDefend, defended: $defended")

    if (o.cards.isEmpty) o.number
    else if (d.cards.isEmpty) d.number
    else {
      state match {
        // passing state
        case 1 =>
          // first card on table
          if (toDefend.isEmpty) {
            //println("first card: player "+ o.number +" plays "+ o.cards.head)
            play(d, new Player(o.cards.tail, o.number), tr, o.cards.head::toDefend, Nil, state)
          }
          // other passes
          else {
            //println("other passes?")
            val passRank = toDefend.head.rank
            val (removed, remaining) = o.removeFirstOfRank(passRank)
            removed match {
              // can pass
              case Some(x) =>
                //println("player "+ o.number + " passes with "+ x)
                play(d, new Player(remaining, o.number), tr, x::toDefend, Nil, state)
              // no card to pass, next state
              case None =>
                //println("player "+ o.number + " cannot pass")
                play(d, o, tr, toDefend, Nil, state+1)
            }
          }
        // defending state, d starts defending
        case 2 =>
          toDefend match {
            // all defended, defender attacks in 1 stage
            case Nil =>
              //println("all defended, defender attacks in state 1")
              play(d, o, tr, Nil, Nil, state-1)
            // cards left to defend, defender tries to defend
            case cs =>
              //println("there are cards to defend for player "+ d.number)
              val (defender, remaining) = d.getDefenceCards(cs, tr)
              //println("remaining: "+ remaining + ", defender: "+ defender)
              defender match {
                // cannot defend, defender takes all cards and starts offence in state 1
                case x if x.length < cs.length =>
                  //println("player "+ d.number + " cannot defend")
                  val pl = new Player(d.cards ::: toDefend ::: defended, d.number)
                  play(o, new Player(pl.sortCards(trump), pl.number), tr, Nil, Nil, state-1)
                // can defend, defends, go to stage 3 when attacker can add more cards
                case x if x.length == cs.length =>
                  //println("player "+ d.number + " defended all cards")
                  play(o, new Player(remaining, d.number), tr, Nil, x:::cs:::defended, state+1)

              }
          }
        // attacker tries to add cards
        case 3 =>
          //println("player "+ o.number + " tries to add cards")
          val (toAdd, atHand) = o.addCards(defended:::toDefend, d.cards.length, trump)
          val pl = new Player(atHand, o.number)
          play(pl, d, tr, toAdd:::toDefend, defended, state-1)
      }
    }
  }



}
