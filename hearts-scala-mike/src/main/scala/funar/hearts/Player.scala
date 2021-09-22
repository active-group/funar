package funar.hearts

import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

case class Player(id: String, name: String) {
  override def equals(other: Any): Boolean =
    other match {
      case other: Player => this.id == other.id
      case _ => false
    }

  override def hashCode: Int = this.id.hashCode
}

object Player {
  import GameEvent._

  type Pile = Set[Card]
  case class PlayerState[PlayerId](hand: Hand, trick: Trick, pile: Pile)

  def emptyPlayerState[PlayerId] = PlayerState[PlayerId](Hand.empty, Trick.empty, Set.empty)

  def playerProcessEvent[PlayerId](player: Player, event: GameEvent, state: PlayerState[PlayerId]): PlayerState[PlayerId] =
    event match {
      case HandDealt(player1, hand) =>
        if (player == player1)
          PlayerState(hand = hand, trick = Trick.empty, pile = Set.empty)
        else
          state
      case PlayerTurnChanged(player1) => state
      case LegalCardPlayed(player1, card) =>
        if (player == player1)
          state.copy(hand = state.hand - card,
                     trick = Trick.add(state.trick, player1, card))
        else
          state.copy(trick = Trick.add(state.trick, player1, card))
      case IllegalCardPlayed(player1, card) => state
      case TrickTaken(player1, trick) =>
        if (player1 == player)
          state.copy(trick = Trick.empty,
                     pile = state.pile ++ Trick.cards(trick))
        else
          state.copy(trick = Trick.empty)
      case GameEnded(winner) => state
    }

    // Eff: free monad from the eff library
    type EventProcessor[Effects, Event, Command] = Event => Eff[Effects, Seq[Command]]

    type Strategy[Effects] = Player => EventProcessor[Effects, GameEvent, GameCommand]

}