package funar.hearts

// Event / "Ereignis"
// (Beschreibung von) etwas, was passiert ist --> Vergangenheit

// Event-Sourcing
// Events bilden die bestandsführenden Daten

// - Events sollten alles erzählen
// - Events sollten fachlich motiviert sein
// - Redundanz ist vollkommen in Ordnung

// Command
// Wunsch, dass etwas passieren soll
// != Event

/*
enum GameEvent {
  case PlayerPlayed(player: Player, card: Card, positionInTrick: Int)
  case PlayerWasDealt(player: Player, hand: Hand)
  case PlayerTookTrick(player: Player, cards: Set[Card])
  case PointsCounted(points: Map[Player, Int])
  case PlayersTurn(player: Player)
  case NewGameStarted(players: List[Player])
  case GameEnded(winner: Player)
  case NewRoundStarted(number: Int)
}

enum GameCommand {
  case PlayCard(player: Player, card: Card)
}
*/

enum GameEvent {
  case HandDealt(player: Player, hand: Hand)
  case PlayerTurnChanged(player: Player)
  case LegalCardPlayed(player: Player, card: Card)
  case IllegalCardPlayed(player: Player, card: Card)
  case TrickTaken(player: Player, trick: Trick)
  case GameEnded(won: Player)
}

enum GameCommand {
  case DealHands(hands: PlayerHands)
  case PlayCard(player: Player, card: Card)
}