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

  // Seq: generalisierte Folgen // analog zu List<A> in Java
  // Seq: Interface
  // List: konkrete Implementierung 
  // vgl. List<A> Interface, Implementierung ArrayList<A>
  def cartesianProduct[A, B](as: Seq[A], bs: Seq[B]): Seq[(A, B)] =
    as.map { a => bs.map { b => (a, b) }}.flatten

  val deck : Seq[Card] = cartesianProduct(Suit.all, Rank.all).map { case (suit, rank) => Card(suit, rank) }
}

type Hand = Set[Card]

object Hand {
  val empty: Hand = Set.empty
}

// letzte Karte ist das erste Element
type Trick = List[(Player, Card)]

object Trick {
  val empty: Trick = List.empty

  val isEmpty(trick: Trick): Boolean = trick.isEmpty
  
  // def isEmpty(trick: Trick): Boolean = trick.isEmpty

  def cards(trick: Trick): Seq[Card] = trick.map(_._2) // 2. Komponente aus Tupel

  def add(trick: Trick, player: Player, card: Card) =
    (player, card) :: trick

  def leadingCard(trick: Trick): Card =
    trick.last._2

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

  def prettyPlayer(player: Player): String = player.name ++ "(" ++ player.id ++ ")"

  def prettyCard(card: Card): String =
    card.rank.toString ++ " of " ++ card.suit.toString

}


type PlayerHands = Map[Player, Hand]

