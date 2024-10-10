package funar.hearts

import cats._
import cats.implicits._
import cats.data._
import cats.effect.IO
import cats.effect.std.Random
import cats.Traverse
import cats.Foldable

object Sync {
  import Cards._
  import Table._
  import Strategy._
  import Free._

  type IOStrategy = GameEvent => IO[Seq[GameCommand]]

  def gameLoop(player1: Player, strategy1: IOStrategy,
               player2: Player, strategy2: IOStrategy,
               player3: Player, strategy3: IOStrategy,
               player4: Player, strategy4: IOStrategy) : IO[Option[Player]] = {
    val players = List(player1, player2, player3, player4)
    Random.scalaUtilRandom[IO] >>= { random =>
      random.shuffleList(Card.deck.toList) >>= { shuffledDeck =>
        val game = TableIO.tableIO(players)
        def loop(commands: Seq[GameCommand]): IO[Option[Player]] = {
          println("commands: " + commands)
          if (commands.isEmpty)
            IO.pure(None)
          else
            commands.map(game).sequence.map(_.flatten) >>= { events =>
              println("events: " + events)
              GameEvent.eventsWinner(events) match {
                case Some(winner) => IO.pure(Some(winner))
                case None =>
                  events.foldM(Seq.empty[GameCommand]) { (commands, event) =>
                    for {
                      commands1 <- strategy1(event)
                      commands2 <- strategy2(event)
                      commands3 <- strategy3(event)
                      commands4 <- strategy4(event)
                    } yield (commands ++ commands1 ++ commands2 ++ commands3 ++ commands4)
                  } >>= loop
              }
            }
        }
        val hands = players.zip(Shuffle.distribute(players.length, shuffledDeck).map(Hand.make(_))).toMap
        loop(Seq(GameCommand.DealHands(hands)))
      }
    }
  }

  def gameAlong(): Option[Player] = {
    val player1 = Player("Mike")
    val strategy1 = alongStrategy[StatePlayer](player1)
    val player2 = Player("Peter")
    val strategy2 = alongStrategy[StatePlayer](player2)
    val player3 = Player("Nicole")
    val strategy3 = alongStrategy[StatePlayer](player3)
    val player4 = Player("Annette")
    val strategy4 = alongStrategy[StatePlayer](player4)
    val playerIO1 = statePlayerIO(player1, strategy1)
    val playerIO2 = statePlayerIO(player2, strategy2)
    val playerIO3 = statePlayerIO(player3, strategy3)
    val playerIO4 = statePlayerIO(player4, strategy4)
    import cats.effect.unsafe.implicits.global
    gameLoop(player1, playerIO1,
             player2, playerIO2,
             player3, playerIO3,
             player4, playerIO4).unsafeRunSync()
  }

  def gameInteractive(): Option[Player] = {
    val player1 = Player("Mike")
    val strategy1 = interactiveStrategy[StateTtyPlayer](player1)
    val player2 = Player("Peter")
    val strategy2 = interactiveStrategy[StateTtyPlayer](player2)
    val player3 = Player("Nicole")
    val strategy3 = interactiveStrategy[StateTtyPlayer](player3)
    val player4 = Player("Annette")
    val strategy4 = interactiveStrategy[StateTtyPlayer](player4)
    val playerIO1 = stateTtyPlayerIO(player1, strategy1)
    val playerIO2 = stateTtyPlayerIO(player2, strategy2)
    val playerIO3 = stateTtyPlayerIO(player3, strategy3)
    val playerIO4 = stateTtyPlayerIO(player4, strategy4)
    import cats.effect.unsafe.implicits.global
    gameLoop(player1, playerIO1,
      player2, playerIO2,
      player3, playerIO3,
      player4, playerIO4).unsafeRunSync()
  }

}