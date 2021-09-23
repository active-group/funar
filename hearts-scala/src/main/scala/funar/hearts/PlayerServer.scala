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

object PlayerServer {

  // Needed by `BlazeServerBuilder`. Provided by `IOApp`.
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  import Player._

  def ioPlayer[PlayerId](player: Player, strategy: Strategy[Fx.fx1[State[PlayerState[PlayerId], *]]])
      : GameEvent => IO[Seq[GameCommand]] = {
    var playerState: PlayerState[PlayerId] = emptyPlayerState
    def processEvent(event: GameEvent): IO[Seq[GameCommand]] = IO {
      val (commands, newPlayerState )= strategy(player)(event).runState[PlayerState[PlayerId]](playerState).run
      playerState = newPlayerState
      commands
    }
    processEvent(_)
  }

  type Effects[PlayerId] = Fx.fx1[State[PlayerState[PlayerId], *]]

  def playerApp[PlayerId](player: Player, strategy: Strategy[Effects[PlayerId]]) = {
    val playerProcessEvent = ioPlayer[PlayerId](player, strategy)
    HttpRoutes.of[IO] {
      case req @ POST -> Root / "event" =>
        for {
          eventJson <- req.as[Json]
          eventResult = HeartsJson.gameEventDecoder(eventJson)
          resp <- eventResult match {
                    case Left(error) => BadRequest("could not decode event")
                    case Right(event) =>
                      for {
                        commands <- playerProcessEvent(event)
                        resp <- Ok(HeartsJson.encodeGameCommands(commands))
                      } yield resp
                  }
        } yield resp
    }.orNotFound    
  }

  def server[PlayerId](port: Int, player: Player, strategy: Strategy[Effects[PlayerId]]) = 
    BlazeServerBuilder[IO](global)
    .bindHttp(port)
    .withHttpApp(CORS(playerApp[PlayerId](player, strategy)))
    .resource

  def fiber[PlayerId](port: Int, player: Player, strategy: Strategy[Effects[PlayerId]]) =
    server[PlayerId](port, player, strategy).use(_ => IO.never).start.unsafeRunSync()

  def alongFiber[PlayerId](port: Int, player: Player) =
    fiber[PlayerId](port, player, alongStrategy[PlayerId, Effects[PlayerId]])
}

object PlayerClient {
  import org.http4s.client.dsl.io._
  import org.http4s.client.blaze._
  import cats.effect.IO
  import io.circe.generic.auto._
  import fs2.Stream

  // Needed by `BlazeClientBuilder`. Provided by `IOApp`.
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  // Decode the Hello response
  def eventClient(msg: String): Stream[IO, Json] = {
    // Encode a User request
    val req = POST(Json.fromString(msg), uri"http://localhost:8080/event")
    // Create a client
    BlazeClientBuilder[IO](global).stream.flatMap { httpClient =>
      Stream.eval(httpClient.expect(req)(jsonOf[IO, Json]))
    }
  }

}