package funar.hearts

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