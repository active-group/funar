package funar.hearts

import cats.Traverse
import cats.data._
import cats.implicits._
import cats.effect.IO

object TableIO {
  import Table._
  def tableIO(players: List[Player]): GameCommand => IO[Seq[GameEvent]] = {
    var ref = (tableLoopM, emptyTableState(players))
    def processCommand(command: GameCommand): IO[Seq[GameEvent]] = IO {
      val (next, state) = ref
      val (state1, events, step) = runTable(next(command), state, Seq.empty)
      step match {
        case Left(cont) => ref = (cont, state1)
        case Right(_) => ()
      }
      events
    }
    processCommand
  }
}