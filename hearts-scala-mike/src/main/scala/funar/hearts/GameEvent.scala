package funar.hearts

// event: something that happened n the past
// the sequence of events: describe everything that happened
// redundancy is OK
// design from perspective of originating / your domain
// assume that they live forever

// command: something that somebody wants to happen in the future
// != event
/*
sealed trait GameEvent
case class CardPlayed(player: Player, card: Card) extends GameEvent
case class CardsDealt(hand1: Hand, hand2: Hand, hand3: Hand, hand4: Hand) extends GameEvent
case class CardsShuffled(shuffledDeck: List[Card]) extends GameEvent
case class TrickEnded(player: Player, trick: Trick) extends GameEvent
case class TrickStarted(nextPlayer: Player) extends GameEvent
case class GameEnded(winner: Player, loser: Player) extends GameEvent

sealed trait GameCommand
case class PutCardOnTrick(player: Player, card: Card) extends GameCommand
case class DealCards(shuffledDeck: List[Card]) extends GameCommand
case class ShuffleCards() extends GameCommand
case class PickupCards(player: Player, cards: Set[Card]) extends GameEvent
*/

sealed trait GameEvent
object GameEvent {
  case class HandDealt(player: Player, hand: Hand) extends GameEvent
  case class PlayerTurnChanged(player: Player) extends GameEvent
  case class LegalCardPlayed(player: Player, card: Card) extends GameEvent
  case class IllegalCardPlayed(player: Player, card: Card) extends GameEvent
  case class TrickTaken(player: Player, trick: Trick) extends GameEvent
  case class GameEnded(won: Player) extends GameEvent
}

sealed trait GameCommand
object GameCommand {
  case class DealHands(hands: PlayerHands) extends GameCommand
  case class PlayCard(player: Player, card: Card) extends GameCommand
}

