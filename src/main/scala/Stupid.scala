object Game {

}

class Game {
  val suites = new Hearts() :: new Diamonds() :: new Clubs() ::  new Spades() :: Nil
  val ranks = 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: Nil
  val trumpSuite = None

  def play(trump: Suite, cards: List[List[String]]): Int = {
    ???
  }
}
