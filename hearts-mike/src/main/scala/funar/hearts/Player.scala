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

  def playerProcessEventM[PlayerId, Effects](player: Player, event: GameEvent)
          (implicit member: State[PlayerState[PlayerId], _] |= Effects): Eff[Effects, Unit] =
    for {
      playerState <- get[Effects, PlayerState[PlayerId]]
      playerState1 = playerProcessEvent(player, event, playerState)
      _ <- put[Effects, PlayerState[PlayerId]](playerState1)
    } yield ()

  type EventProcessor[Effects, Event, Command] = Event => Eff[Effects, Seq[Command]]

  type Strategy[Effects] = Player => EventProcessor[Effects, GameEvent, GameCommand]

  type Chooser[PlayerId, Effects] = (Player, PlayerState[PlayerId]) => Eff[Effects, Card]

  def chooserStrategy[PlayerId, Effects](chooser: Chooser[PlayerId, Effects])
          (implicit member: State[PlayerState[PlayerId], _] |= Effects): Strategy[Effects] = {
    import GameCommand._
    (player: Player) => 
      (event: GameEvent) =>
        for {
          _ <- playerProcessEventM(player, event)
          playerState <- get[Effects, PlayerState[PlayerId]]
          commands <- event match {
            case HandDealt(player1, hand) => {
              println("player " ++ player.toString ++ " received HandDealt for player " ++ player1.toString ++ " (" ++ (player == player1).toString ++ ")")
              if ((player == player1) && hand.contains(Card(Suit.Clubs, Rank.Two)))
                Eff.pure[Effects, Seq[GameCommand]](Seq(PlayCard(player, Card(Suit.Clubs, Rank.Two))))
              else
                Eff.pure[Effects, Seq[GameCommand]](Seq.empty)
            }
            case PlayerTurnChanged(player1) =>
              if (player == player1)
                for { card <- chooser(player, playerState) } yield Seq(PlayCard(player, card))
              else
                Eff.pure[Effects, Seq[GameCommand]](Seq.empty)
            case LegalCardPlayed(player1, card) => Eff.pure[Effects, Seq[GameCommand]](Seq.empty)
            case IllegalCardPlayed(player1, card) => Eff.pure[Effects, Seq[GameCommand]](Seq.empty)
            case TrickTaken(player1, trick) => Eff.pure[Effects, Seq[GameCommand]](Seq.empty)
            case GameEnded(winner) => Eff.pure[Effects, Seq[GameCommand]](Seq.empty)
          }

        } yield commands
  }

  def chooseAlong[PlayerId, Effects]: Chooser[PlayerId, Effects] = {
    (_, playerState) =>
    if (Trick.isEmpty(playerState.trick))
      Eff.pure[Effects, Card](playerState.hand.minBy(_.rank))  
    else {
      val firstCard = Trick.leadingCard(playerState.trick)
      val followingCardsOnHand = playerState.hand.filter { _.suit == firstCard.suit }
      followingCardsOnHand.minByOption(_.rank) match {
        case None => Eff.pure[Effects, Card](playerState.hand.maxBy(_.rank))
        case Some(card) => Eff.pure[Effects, Card](card)
      }
    } 
  }

  def alongStrategy[PlayerId, Effects](implicit member: State[PlayerState[PlayerId], _] |= Effects): Strategy[Effects] =
    chooserStrategy(chooseAlong[PlayerId, Effects])

  def chooseInteractive[Effects, PlayerId](implicit member: Teletype |= Effects): Chooser[PlayerId, Effects] = {
    (player, playerState) =>
      for {
        _ <- Teletype.writeTTY("Your turn, player " ++ player.name)
        handList = playerState.hand.toList
        _ <- if (Trick.isEmpty(playerState.trick))
                Teletype.writeTTY("You lead the next trick.")
             else
                Teletype.writeTTY("Trick: " ++ Trick.pretty(playerState.trick))
        _ <- Teletype.writeTTY("Your hand:\n" ++ prettyCards(handList))
        ncards = playerState.hand.size
        _ <- Teletype.writeTTY("Pick a card (1-"  ++ ncards.toString ++ ")")
        selected <- getNumber(1, ncards)
      } yield handList(selected-1)
  }

  def getNumber[Effects](lo: Int, hi: Int)(implicit member: Teletype |= Effects): Eff[Effects, Int] =
    for {
      s <- Teletype.readTTY
      input = s.toInt
      n <- if ((lo <= input) && (input <= hi))
             Eff.pure[Effects, Int](input)
           else
            for {
              _ <- Teletype.writeTTY("Input must be between " ++ lo.toString ++ " and " ++ hi.toString ++ ". Try again.")
              n <- getNumber(lo, hi)
            } yield n
    } yield n

  def prettyCards(cards: List[Card]): String = {
    def prettyOne(n: Integer, card: Card): String = "(" ++ n.toString ++ ")" ++ Trick.prettyCard(card)
    def loop(n: Integer, cards: List[Card]): String =
      cards match {
        case Nil => ""
        case List(card) => prettyOne(n, card)
        case card::cards => prettyOne(n, card) ++ "\n" ++ loop(n+1, cards)
      }
    loop(1, cards)
  }

  def interactiveStrategy[PlayerId, Effects]
    (implicit ttyState: Teletype |= Effects,
              memberState: State[PlayerState[PlayerId], _] |= Effects): Strategy[Effects] =
    chooserStrategy(chooseInteractive)


  
}

