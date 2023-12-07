package funar.hearts

import scala.annotation.tailrec
import funar.hearts.GameEvent.HandDealt
import funar.hearts.GameEvent.PlayerTurnChanged

import Cards._

object Table {
  def legalCard(card: Card, hand: Hand, trick: Trick): Boolean =
    Hand.containsCard(hand, card) &&
    (Trick.isEmpty(trick) ||
     {
       val firstCard = Trick.leadingCard(trick)
       val firstSuit = firstCard.suit
       (card.suit == firstSuit || Hand.cards(hand).forall(_.suit != firstSuit))
     })

  def cardScore(card: Card): Integer =
    card match {
      case Card(Suit.Spades, Rank.Queen) => 13
      case Card(Suit.Hearts, _) => 1
      case _ => 0
    }

  type PlayerHands = Map[Player, Hand]

  type PlayerPiles = Map[Player, Pile]

  case class TableState(players: List[Player],
                        hands: PlayerHands,
                        piles: PlayerPiles,
                        trick: Trick)

  def emptyTableState(players: List[Player]): TableState =
    TableState(players = players,
               hands = Map.from(players.map((_, Hand.empty))),
               piles = Map.from(players.map((_, Pile.empty))),
               trick = Trick.empty)

  def gameAtBeginning(tableState: TableState): Boolean =
    Trick.isEmpty(tableState.trick) &&
      tableState.piles.values.forall(Pile.isEmpty)

  def playerAfter(tableState: TableState, player: Player): Player =
    rotate(rotateTo(player, tableState.players)).head

  def rotate[A](list: List[A]): List[A] = {
    assert(!list.isEmpty)
    list.tail ++ List(list.head)
  }

  @tailrec
  def rotateTo[A](x: A, list: List[A]): List[A] = {
    assert(!list.isEmpty)
    if (list.head == x)
      list
    else
      rotateTo(x, rotate(list))
  }

  def currentPlayer(tableState: TableState): Player =
    tableState.players.head

  def playValid(tableState: TableState, player: Player, card: Card): Boolean = {
    val hand = tableState.hands(player)
    val trick = tableState.trick
    legalCard(card, hand, trick) &&
     (if (gameAtBeginning(tableState))
        card == Card(Suit.Clubs, Rank.Two)
      else
        currentPlayer(tableState) == player)
  }

  def turnOver(tableState: TableState): Boolean =
    tableState.players.length == tableState.trick.length

  def whoTakesTrick(trick: Trick): Option[Player] = {
    trick match {
      case Nil => None
      case (player0, card0) :: rest1 =>
        @tailrec
        def loop(player: Player, card: Card, trick: Trick): Player =
          trick match {
            case Nil => player
            case ((player1, card1)::rest) =>
              card.beats(card1) match {
                case None => loop(player, card, rest)
                case Some(false) => loop(player, card, rest)
                case Some(true) => loop(player1, card1, rest)
              }
          }
        Some(loop(player0, card0, rest1))
    }
  }

  def turnOverTrick(tableState: TableState): Option[(Trick, Player)] =
    if (turnOver(tableState))
      whoTakesTrick(tableState.trick).map((tableState.trick, _))
    else 
      None

  def pileScore(pile: Pile): Integer =
    Pile.cards(pile).map(cardScore).foldLeft(0)(_ + _)

  def gameOver(tableState: TableState): Option[Player] =
    if (tableState.hands.values.forall(Hand.isEmpty)) {
      val playerScores = tableState.piles.map { case (player, pile) => (player, pileScore(pile)) }
      Some(playerScores.minBy(_._2)._1)
    } else 
      None

  def takeCard(hands: PlayerHands, player: Player, card: Card): PlayerHands =
    hands.updatedWith(player) { o => o.map(Hand.removeCard(_, card)) }

  def addTrickToPile(playerPiles: PlayerPiles, player: Player, trick: Trick): PlayerPiles = {
    val playerPile = playerPiles.getOrElse(player, Pile.empty)
    playerPiles.updated(player, Pile.addTrick(playerPile, trick))
  }

  def dealHand(player: Player, hand: Hand, hands: PlayerHands): PlayerHands =
    hands.updated(player, hand)

  def tableProcessEvent(event: GameEvent, tableState: TableState): TableState =
    event match {
      case HandDealt(player, hand) => 
        tableState.copy(hands = dealHand(player, hand, tableState.hands),
                        trick = Trick.empty)
      case PlayerTurnChanged(player) =>
        tableState.copy(players = rotateTo(player, tableState.players))
      case GameEvent.LegalCardPlayed(player, card) =>
        tableState.copy(hands = takeCard(tableState.hands, player, card),
                        trick = Trick.add(tableState.trick, player, card))
      case GameEvent.IllegalCardAttempted(player, card) => tableState
      case GameEvent.TrickTaken(player, trick) =>
        tableState.copy(piles = addTrickToPile(tableState.piles, player, trick),
                        trick = Trick.empty)
      case GameEvent.GameEnded(player) => tableState
    }

  @tailrec
  def runTable[A](game: GameM.Game[A], state: TableState, events: Seq[GameEvent])
      : (TableState, Seq[GameEvent], Either[GameCommand => GameM.Game[A], A]) = {
    import GameM.Game._
    game match {
      case PlayValid(player, card, cont) =>
        runTable(cont(playValid(state, player, card)), state, events)
      case TurnOverTrick(cont) =>
        runTable(cont(turnOverTrick(state)), state, events)
      case PlayerAfter(player, cont) =>
        runTable(cont(playerAfter(state, player)), state, events)
      case GameOver(cont) =>
        runTable(cont(gameOver(state)), state, events)

      case RecordEvent(event, cont) =>
        runTable(cont(()), tableProcessEvent(event, state), events.appended(event))
      case GetCommand(cont) =>
        (state, events, Left(cont))
      case Done(result) =>
        (state, events, Right(result))
    }
  }

}   
