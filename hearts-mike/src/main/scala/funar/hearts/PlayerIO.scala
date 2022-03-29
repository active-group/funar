package funar.hearts

import cats.effect._
import cats.data._
import cats.syntax.all._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object PlayerIO {
  import Player._

  def playerIO[PlayerId](player: Player, strategy: Strategy[Fx.fx1[State[PlayerState[PlayerId], _]]])
      : GameEvent => IO[Seq[GameCommand]] = {
    var playerState: PlayerState[PlayerId] = emptyPlayerState
    def processEvent(event: GameEvent): IO[Seq[GameCommand]] = IO {
      val (commands, newPlayerState) = strategy(player)(event).runState[PlayerState[PlayerId]](playerState).run
      playerState = newPlayerState
      commands
    }
    processEvent(_)
  }

}