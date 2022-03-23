package funar.hearts

object `package` {
  type Hand = Set[Card]

  type Trick = List[(Player, Card)]

  type PlayerHands = Map[Player, Hand]
}

