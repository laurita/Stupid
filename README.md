The Fool
======

A simplified version of a Lithuanian/Russian card game called "The Fool". 

## Rules

There's a Lithuanian/Russian card game called "The Fool".

It has these (simplified) rules:

Suites: Hearts, Diamonds, Clubs, Spades.
Card ranks: 2, 3, 4, 5, 6, 7, 8, 9, T, Jack, Queen, King, Ace.

Cards are ordered by their rank and then their suite.

One suite is called the trump suite. Any trump card is always higher than any non-trump card.

The trump suite is represented as the first line in data file by a letter: H|D|C|S.

Two players get their cards which are represented by a line in the file:
ST D2 CJ HQ DA | H2 D3 C4 S5 H6 D7 C8 S9

First player starts as offense. Offense always starts.

1) He plays smallest non-trump card. If he does not have non-trump card, he plays smallest trump card then.

2) If second player has cards of the same rank, he plays smallest of them and passes the turn to P1. Roles of the attacker/defender switch then.

3) Whenever there is no card to be passed, defender must use smallest non-trump cards that are bigger than the each card on the table to cover them, starting with the smallest uncovered cards currently on table. If he doesn't have a non-trump card to cover any card, he can use a trump card to do that. If he doesn't have any card to cover current card, he shouldn't play any more cards in this turn.

4) Attacker then can add more cards that are of the same rank that currently exist on the table for defender to current. Maximum number of non-covered cards on the table cannot exceed defenders current hand size. If such scenario arises, attacker then plays the smallest card from their hand.

5a) If all cards are covered - defender wins this round, cards are discarded and defense becomes offense for the next turn.

5b) If not all cards are covered - go to 3). If defender can't cover all cards it loses the round and takes all cards to his hand. It's offense turn.

Repeat this until one player does not have cards in his hand after a turn.
That player wins.

## Running the program

1. Fork the repository
2. Clone it
3. cd to the project directory
4. run `mvn clean compile assembly:single`
5. run `java -cp the_fool-1.0-SNAPSHOT-jar-with-dependencies.jar Main <input_file>`

