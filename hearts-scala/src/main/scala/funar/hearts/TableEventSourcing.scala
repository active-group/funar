package funar.hearts

import cats._
import cats.implicits._
import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.Members.{&:, &&:, extractMember}


object TableEventSourcing {
  import Table._
  import EventSourcing._

  type _tableState[Effects] = State[TableState, *] |= Effects
  type _gameEventSourcing[Effects] = EventSourcing[GameEvent, *] |= Effects

  type _tableEventSourcing[Effects] = _tableState[Effects] &&: _gameEventSourcing[Effects]

  def playerHandM[Effects : _tableState](player: Player): Eff[Effects, Hand] =
    for {
      state <- get[Effects, TableState]
    } yield state.hands(player)

  def playerStackM[Effects: _tableState](player: Player): Eff[Effects, Hand] =
    get[Effects, TableState].map(_.hands(player))

  def trickM[Effects: _tableState]: Eff[Effects, Trick] =
    get[Effects, TableState].map(_.trick)
  
  def processGameEventM[Effects: _tableEventSourcing](event: GameEvent): Eff[Effects, Unit] =
    for {
      _ <- recordEvent(event)
      _ <- modify(tableProcessEvent(event, _))
    } yield ()

  def whoTakesTrickM[Effects: _tableState]: Eff[Effects, (Player, Trick)] =
    get[Effects, TableState].map { state => (whoTakesTrick(state.trick), state.trick) }

  def turnOverM[Effects: _tableState]: Eff[Effects, Boolean] =
    get[Effects, TableState].map(turnOver(_))

  def gameOverM[Effects: _tableState]: Eff[Effects, Option[Player]] =
    get[Effects, TableState].map(gameOver(_))

  def playValidM[Effects: _tableState](player: Player, card: Card): Eff[Effects, Boolean] =
    get[Effects, TableState].map(playValid(_, player, card))

  def processGameCommandM[Effects: _tableEventSourcing](command: GameCommand): Eff[Effects, Unit] =
    for {
      state <- get[Effects, TableState]
      events = tableProcessCommand(command, state)
      _ <- events.traverseTap(processGameEventM[Effects])
    } yield ()

  def gameCommandEventsM[Effects: _tableEventSourcing](command: GameCommand): Eff[Effects, Seq[GameEvent]] =
    for {
      state <- get[Effects, TableState]
      _ <- checkPointEvent[GameEvent, Effects]
      _ <- processGameCommandM(command)
      events <- checkPointEvent[GameEvent, Effects]
    } yield events
}