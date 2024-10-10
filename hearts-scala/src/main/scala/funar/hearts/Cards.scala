package funar.hearts

enum Suit {
  case Diamonds
  case Clubs
  case Spades
  case Hearts
}

object Suit {
  val all = Seq(Diamonds, Clubs, Spades, Hearts)
}

enum Rank {
  case Two
  case Three
  case Four
  case Five
  case Six
  case Seven
  case Eight
  case Nine
  case Ten
  case Jack
  case Queen
  case King
  case Ace
}

object Rank {
  given ordering: Ordering[Rank] = Ordering.by(_.ordinal)

  val all: Seq[Rank] = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, 
                           Jack, Queen, King, Ace)
}

case class Card(suit: Suit, rank : Rank) {
  // Eine Karte kann nur eine andere Karte gleicher Farbe nach Wert schlagen
  def beats(other: Card): Option[Boolean] =
    if (this.suit == other.suit) {
      import Rank.ordering
      Some(Ordering[Rank].gt(this.rank, other.rank))
    } else
      None
}

object Card {

  def cartesianProduct[A, B](as: Seq[A], bs: Seq[B]): Seq[(A, B)] =
    as.map { a => bs.map { b => (a, b) }} flatten

  val deck : Seq[Card] = cartesianProduct(Suit.all, Rank.all).map { case (suit, rank) => Card(suit, rank) }
}

case class Player(name: String)

object Cards {

  opaque type Hand = Set[Card]
  object Hand {

    def make(cards: Iterable[Card]): Hand = cards.toSet
    
    val empty: Hand = Set.empty

    def isEmpty(hand: Hand): Boolean = hand.isEmpty

    def cards(hand: Hand): Seq[Card] = hand.toSeq

    def containsCard(hand: Hand, card: Card): Boolean =
      hand.contains(card)

    def removeCard(hand: Hand, card: Card): Hand =
      hand - card
  }

  type Trick = List[(Player, Card)]
  object Trick {
    val empty: Trick = List.empty

    def isEmpty(trick: Trick): Boolean = trick.isEmpty

    def cards(trick: Trick): Seq[Card] = trick.map(_._2)

    def add(trick: Trick, player: Player, card: Card) =
      (player, card) +: trick

    def leadingCard(trick: Trick): Card =
      trick.last._2

    def containsCard(trick: Trick, card: Card): Boolean =
      trick.contains(card)

    def pretty(trick: Trick) = {
      def prettyOne(player: Player, card: Card): String = 
        prettyCard(card) ++ " from " ++ prettyPlayer(player)
      def p(s: Trick): String =
        s match {
          case Nil => ""
          case Seq((player, card)) => prettyOne(player, card)
          case ((player, card)::rest) => prettyOne(player, card) ++ ", " ++ p(rest)
        }
      p(trick.reverse)
    }

    def prettyPlayer(player: Player): String = player.name

    def prettyCard(card: Card): String =
      card.rank.toString ++ " of " ++ card.suit.toString

  }

  opaque type Pile = Set[Card]
  object Pile {
    val empty: Pile = Set.empty

    def isEmpty(pile: Pile): Boolean = pile.isEmpty

    def addTrick(pile: Pile, trick: Trick): Pile =
      pile.union(Trick.cards(trick).toSet)

    def cards(pile: Pile): Seq[Card] = pile.toSeq
  }

}
