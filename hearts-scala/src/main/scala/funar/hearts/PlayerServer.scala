package funar.hearts

import cats.effect._
import cats.data._
import cats.syntax.all._
import cats.effect.unsafe.IORuntime

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.blaze.server._
import org.http4s.implicits._
import org.http4s.server.middleware._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.concurrent.ExecutionContext.Implicits.global

object PlayerServer {

  import Player._

  type Effects[PlayerId] = Fx.fx1[State[PlayerState[PlayerId], _]]

  def playerApp[PlayerId](player: Player, strategy: Strategy[Effects[PlayerId]]) = {
    val playerProcessEvent = PlayerIO.playerIO[PlayerId](player, strategy)
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
    BlazeServerBuilder[IO]
    .bindHttp(port)
    .withHttpApp(CORS.policy(playerApp[PlayerId](player, strategy)))
    .resource

  // needed by unsafeRunSync
  import cats.effect.unsafe.implicits.global
  
  def fiber[PlayerId](port: Int, player: Player, strategy: Strategy[Effects[PlayerId]]) =
    server[PlayerId](port, player, strategy).use(_ => IO.never).start.unsafeRunSync()

  def alongFiber[PlayerId](port: Int, player: Player) =
    fiber[PlayerId](port, player, alongStrategy[PlayerId, Effects[PlayerId]])
}

object PlayerClient {
  import org.http4s.client.dsl.io._
  import org.http4s.blaze.client._
  import cats.effect.IO
  import io.circe.generic.auto._
  import fs2.Stream

  // Decode the Hello response
  def eventClient(msg: String): Stream[IO, Json] = {
    // Encode a User request
    val req = POST(Json.fromString(msg), uri"http://localhost:8080/event")
    // Create a client
    BlazeClientBuilder[IO].stream.flatMap { httpClient =>
      Stream.eval(httpClient.expect(req)(jsonOf[IO, Json]))
    }
  }
}