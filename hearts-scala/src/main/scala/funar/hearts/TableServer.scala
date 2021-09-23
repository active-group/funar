package funar.hearts

import cats.effect._

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.middleware._

import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.concurrent.ExecutionContext.Implicits.global

object TableServer {

  // Needed by `BlazeServerBuilder`. Provided by `IOApp`.
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  import Table._

  def ioTable(players: List[Player]): GameCommand => IO[Seq[GameEvent]] = {
    var tableState: TableState = emptyTableState(players)
    def processEvent(command: GameCommand): IO[Seq[GameEvent]] = IO {
      val events = tableProcessCommand(command, tableState)
      tableState = events.foldLeft(tableState) { (tableState: TableState, event: GameEvent) => tableProcessEvent(event, tableState) }
      events
    }
    processEvent(_)
  }

  def tableApp(players: List[Player]) = {
    val tableProcessCommand = ioTable(players)
    HttpRoutes.of[IO] {
      case req @ POST -> Root / "command" =>
        for {
          eventJson <- req.as[Json]
          commandResult = HeartsJson.gameCommandDecoder(eventJson)
          resp <- commandResult match {
                    case Left(error) => BadRequest("could not decode command " + eventJson.toString())
                    case Right(command) =>
                      for {
                        events <- tableProcessCommand(command)
                        resp <- Ok(HeartsJson.encodeGameEvents(events))
                      } yield resp
                  }
        } yield resp
    }.orNotFound    
  }

  def server(port: Int, players: List[Player]) = 
    BlazeServerBuilder[IO](global)
    .bindHttp(port)
    .withHttpApp(CORS(tableApp(players)))
    .resource

  def fiber(port: Int, players: List[Player]) =
    server(port, players).use(_ => IO.never).start.unsafeRunSync()
}
