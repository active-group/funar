package funar.hearts

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.addon.cats.effect._
import org.atnos.eff.syntax.addon.cats.effect._
import cats.Traverse
import cats.data._
import cats.implicits._
import funar.hearts.Table._
import cats.Eval

enum GameStep[+A] {
  case BroadcastEvent(event: GameEvent) extends GameStep[Unit]
  case ReceiveCommand extends GameStep[Option[GameCommand]]
}

enum GameStepResult[A] {
  case EventBroadcast(event: GameEvent, continuation: Unit => GameStepResult[A])
  case WaitingForCommand(continutation: GameCommand => GameStepResult[A])
  case GameDone(result: A)
}

object Game {
  import GameStep._

  type _gameStep[Effects] = GameStep |= Effects
  type _tableState[Effects] = State[TableState, _] |= Effects

  def broadcastEvent[T, R : _gameStep](event: GameEvent): Eff[R, Unit] =
    Eff.send[GameStep, R, Unit](BroadcastEvent(event))

  def receiveCommand[T, R : _gameStep]: Eff[R, Option[GameCommand]] =
    Eff.send[GameStep, R, Option[GameCommand]](ReceiveCommand)

  // ein Event von den Spielern verarbeiten lassen
  def broadcastEvents[Effects: _gameStep](events: Seq[GameEvent]): Eff[Effects, Unit] =
    events.traverse_(broadcastEvent)

  def executeCommand[Effects: _gameStep : _tableState](command: GameCommand): Eff[Effects, (Seq[GameEvent], Option[Player])] =
    for {
      state <- get[Effects, TableState]
      events = tableProcessCommand(command, state)
      state1 = events.foldLeft(state) { (state, event) => tableProcessEvent(event, state) } 
      _ <- put[Effects, TableState](state1)
    } yield (events, gameOver(state1))

  def playLoop[Effects: _gameStep : _tableState](command: GameCommand): Eff[Effects, Option[Player]] =
    for {
      pair <- executeCommand(command)
      (events, maybeWinner) = pair
      _ <- broadcastEvents(events)
      res <- maybeWinner match {
        case None =>
          for {
            maybeCommand <- receiveCommand
            _ = println("received command " ++ maybeCommand.toString)
            res <- maybeCommand match {
              case Some(command) => playLoop(command)
              case None => Eff.pure(None)
            }
          } yield res
        case Some(winner) =>
          Eff.pure(Some(winner))
      }
    } yield res

  def playGame[Effects: _gameStep : _tableState](players: Seq[Player], deck: List[Card]): Eff[Effects, Option[Player]] = {
    val hands = players.zip(distribute(players.length, deck).map(_.toSet)).toMap
    playLoop(GameCommand.DealHands(hands))
  }

  def playOne[A](effects: Eff[Fx1[GameStep], A]): GameStepResult[A] = {
    val interpreter = new Interpreter[GameStep, NoFx, A, GameStepResult[A]] {
      import GameStepResult._
      override def onPure(a: A) = Eff.pure{GameDone(a)}
      override def onEffect[X](x: GameStep[X], continuation: Continuation[NoFx, X, GameStepResult[A]]) =
        x match {
          case BroadcastEvent(event) =>
            Eff.pure(EventBroadcast(event, { _ => continuation(()).run }))
          case ReceiveCommand =>
            Eff.pure(WaitingForCommand({ command => continuation(Some(command)).run }))
        }
      override def onLastEffect[X](x: GameStep[X], continuation: Continuation[NoFx, X, Unit]): Eff[NoFx, Unit] =
        Eff.pure(()) // ??? rilly?
      override def onApplicativeEffect[X, T[_] : Traverse](xs: T[GameStep[X]], continuation: Continuation[NoFx, T[X], GameStepResult[A]]): Eff[NoFx, GameStepResult[A]] = {
        xs.toList match {
          case Nil => continuation(Vector.empty[X].asInstanceOf[T[X]]) // we know it's a vector
          case BroadcastEvent(event)::rest =>
            Eff.pure(EventBroadcast(event,
                      {_ => onApplicativeEffect(rest.toVector, Continuation.lift { (rest: Vector[X]) => continuation((() +: rest).toVector.asInstanceOf[T[X]]) }).run }))
          case ReceiveCommand::rest =>
            Eff.pure(WaitingForCommand({ command => 
              onApplicativeEffect(rest.toVector, Continuation.lift { (rest: Vector[X]) => continuation((Some(command) +: rest).toVector.asInstanceOf[T[X]]) }).run }))
        }
      }
    }
    runInterpreter(effects)(interpreter).run
  }


  def distribute[A](n: Int, list: List[A]): Seq[List[A]] = {
    def extract[A](list: List[A]): List[A] =
      list match {
        case Nil => Nil
        case (x :: xs) => 
          x :: extract(xs.drop(n-1))
      }
    (0 until n).map { i =>
      extract(list.drop(i))
    }
  }
    
}