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
import scala.annotation.tailrec
import cats.effect.IO

object GameIO {
  import Table._ 
  import GameStepResult._

  @tailrec
  private def gatherEvents[A](stepResult: GameStepResult[A], events: Seq[GameEvent]): (Seq[GameEvent], Option[GameCommand => GameStepResult[A]]) =
    stepResult match {
      case EventBroadcast(event, cont) =>
        val stepResult1 = cont(())
        gatherEvents(stepResult1, events :+ event)
      case WaitingForCommand(cont) =>
        (events, Some(cont))
      case GameDone(_) => (events, None)
    }

  def gameIO(players: List[Player]): GameCommand => IO[Seq[GameEvent]] = {
    var ocont: Option[GameCommand => GameStepResult[Option[Player]]] =
      Some { (command: GameCommand) =>
              val game: Eff[Fx.fx2[GameStep, State[TableState, _]], Option[Player]] = Game.playLoop(command)
              val game1 = evalState(emptyTableState(players))(game)
              Game.playOne(game1)
            }
    { (command: GameCommand) =>
      ocont match {
        case None => IO.pure(Seq.empty)
        case Some(cont) => IO {
          val stepResult = cont(command)
          val (events, newCont) = gatherEvents(stepResult, Seq.empty)
          ocont = newCont
          events
        }
      }
    }
  }

}