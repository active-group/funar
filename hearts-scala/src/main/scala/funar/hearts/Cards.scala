package funar.hearts

sealed trait Suit
object Suit {
  case object Diamonds extends Suit
  case object Clubs extends Suit
  case object Spades extends Suit
  case object Hearts extends Suit

  val all = Seq(Diamonds, Clubs, Spades, Hearts)
}

sealed trait Rank extends Ordered[Rank] { 
  val value : Int
  def compare(other: Rank) =
    this.value - other.value
}
object Rank {
  case object Two extends Rank { val value = 2 }
  case object Three extends Rank { val value = 3 }
  case object Four extends Rank { val value = 4 }
  case object Five extends Rank { val value = 5 }
  case object Six extends Rank { val value = 6 }
  case object Seven extends Rank { val value = 7 }
  case object Eight extends Rank { val value = 8 }
  case object Nine extends Rank { val value = 9 }
  case object Ten extends Rank { val value = 10 }
  case object Jack extends Rank { val value = 11 }
  case object Queen extends Rank { val value = 12 }
  case object King extends Rank { val value = 13 }
  case object Ace extends Rank { val value = 14 }

  implicit val ordering: Ordering[Rank] = Ordering.by(_.value)

  val all: Seq[Rank] = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, 
                           Jack, Queen, King, Ace)
}

case class Card(suit: Suit, rank : Rank) {
  // Eine Karte kann nur eine andere Karte gleicher Farbe nach Wert schlagen
  def beats(other: Card): Option[Boolean] =
    if (this.suit == other.suit)
      Some(this.rank > other.rank)
    else
      None
}

object Card {

  def cartesianProduct[A, B](as: Seq[A], bs: Seq[B]): Seq[(A, B)] =
    as.map { a => bs.map { b => (a, b) }} flatten

  val deck : Seq[Card] = cartesianProduct(Suit.all, Rank.all).map { case (suit, rank) => Card(suit, rank) }
}

object Hand {
  val empty: Hand = Set.empty
}

object Trick {
  val empty: Trick = List.empty

  def isEmpty(trick: Trick): Boolean = trick.isEmpty

  def cards(trick: Trick): Seq[Card] = trick.map(_._2)

  def add(trick: Trick, player: Player, card: Card) =
    trick :+ (player, card)

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

  // FIXME: move
  def prettyPlayer(player: Player): String = player.name ++ "(" ++ player.id ++ ")"

  def prettyCard(card: Card): String =
    card.rank.toString ++ " of " ++ card.suit.toString

}

