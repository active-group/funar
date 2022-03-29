package funar.hearts

import cats._
import cats.implicits._
import cats.data._
import cats.effect.IO
import cats.Traverse
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.Members.{&:, &&:, extractMember}
import org.atnos.eff.syntax.addon.cats.effect._
import org.atnos.eff.addon.cats.effect._
import org.atnos.eff.addon.cats.effect.IOEffect._
import org.atnos.eff.interpret._

object Sync {
  import Player._
  import Table._

  type Players[Effects] = Map[Player, Strategy[Effects]] 

  def emptyPlayers[Effects]: Players[Effects] = Map.empty

  def addPlayer[Effects](players: Players[Effects], player: Player, strategy: Strategy[Effects]): Players[Effects] =
    players + (player -> strategy)

  val mike = Player("1", "Mike")
  class Mike() {}
  val nicole = Player("2", "Nicole")
  class Nicole() {}
  val peter = Player("3", "Peter")
  class Peter() {}
  val annette = Player("4", "Annette")
  class Annette() {}
  type _iEffects[Player, R] = (State[PlayerState[Player], _] |= R) &&: (Teletype |= R)
  
  def iMike[Effects: _iEffects[Mike, _]]: Strategy[Effects] = interactiveStrategy[Mike, Effects]
  def iPeter[Effects: _iEffects[Peter, _]]: Strategy[Effects] = interactiveStrategy[Peter, Effects]
  def iNicole[Effects: _iEffects[Nicole, _]]: Strategy[Effects] = interactiveStrategy[Nicole, Effects]
  def iAnnette[Effects: _iEffects[Annette, _]]: Strategy[Effects] = interactiveStrategy[Annette, Effects]

  def iPlayers[Effects: _iEffects[Mike, _] : _iEffects[Peter, _] : _iEffects[Nicole, _] : _iEffects[Annette, _]] =
    addPlayer(addPlayer(addPlayer(addPlayer(emptyPlayers, mike, iMike[Effects]), 
                                                          peter, iPeter[Effects]), 
                                                          nicole, iNicole[Effects]), 
                                                          annette, iAnnette[Effects])

  type _aEffects[Player, R] = State[PlayerState[Player], _] |= R
  def aMike[Effects: _aEffects[Mike, _]]: Strategy[Effects] = alongStrategy[Mike, Effects]
  def aPeter[Effects: _aEffects[Peter, _]]: Strategy[Effects] = alongStrategy[Peter, Effects]
  def aNicole[Effects: _aEffects[Nicole, _]]: Strategy[Effects] = alongStrategy[Nicole, Effects]
  def aAnnette[Effects: _aEffects[Annette, _]]: Strategy[Effects] = alongStrategy[Annette, Effects]

  def aPlayers[Effects: _aEffects[Mike, _] : _aEffects[Peter, _] : _aEffects[Nicole, _] : _aEffects[Annette, _]] =
    addPlayer(addPlayer(addPlayer(addPlayer(emptyPlayers, mike, aMike[Effects]), 
                                                          peter, aPeter[Effects]), 
                                                          nicole, aNicole[Effects]), 
                                                          annette, aAnnette[Effects])

  def playersGameStep[EffectsGameStep, EffectsState, A](players: Players[EffectsState], effects: Eff[EffectsGameStep, A])
        (implicit m: Member.Aux[GameStep, EffectsGameStep, EffectsState],
                  state: State[Seq[GameCommand], _] |= EffectsState): Eff[EffectsState, A] =
    translate(effects)(new Translate[GameStep, EffectsState] {
      def apply[X](step: GameStep[X]): Eff[EffectsState, X] =
        step match {
          case GameStep.BroadcastEvent(event) =>
            players.toList.traverse_ { case (player, strategy) =>
              for { commands <- get[EffectsState, Seq[GameCommand]]
                    newCommands <- strategy(player)(event)
                    _ = println("player " ++ player.toString ++ " upon event " ++ event.toString ++ " produced commands " ++ newCommands.toString)
                    _ <- put(commands ++ newCommands) 
              } yield ()
            }.map(_.asInstanceOf[X])
          case GameStep.ReceiveCommand =>
            for {
              commands <- get[EffectsState, Seq[GameCommand]]
              (command0, commands1) = 
                if (commands.isEmpty)
                  (None, commands)
                else
                  (Some(commands.head), commands.tail)
              _ = println("split commands into " ++ command0.toString ++ " and " ++ commands1.toString);
              _ <- put(commands1)
            } yield command0.asInstanceOf[X]
        }
    })

  def playEvent[Effects](players: Players[Effects], event: GameEvent): Eff[Effects, Seq[GameCommand]] =
    players.toList.foldM[Eff[Effects, _], Seq[GameCommand]](Seq.empty) { (gameCommands: Seq[GameCommand], pair: (Player, Strategy[Effects])) =>
      val (player, playerStrategy) = pair
      for {
        gameCommands1 <- playerStrategy(player)(event)
      } yield (gameCommands ++ gameCommands1)
    }

  def gameAlong(): Option[Player] = {
    type PlayerEffects = Fx.fx4[State[PlayerState[Mike], _], State[PlayerState[Peter], _], 
                                State[PlayerState[Nicole], _], State[PlayerState[Annette], _]]
    type Effects1 = Fx.prepend[State[TableState, _], PlayerEffects]
    type Effects2 = Fx.prepend[State[Seq[GameCommand], _], Effects1]
    type Effects = Fx.prepend[GameStep, Effects2]
    val players = aPlayers[Effects2]
    val playerIdentities = players.keys.toSeq
    val game = Game.playGame[Effects](playerIdentities, Card.deck.toList)
    val gameWithPlayers = playersGameStep[Effects, Effects2, Option[Player]](players, game)
    val s1 = gameWithPlayers.evalState(Seq.empty)
    val s2 = s1.evalState(emptyTableState(players.keys.toList))
    val s3 = s2.evalState(emptyPlayerState)
    val s4 = s3.evalState(emptyPlayerState)
    val s5 = s4.evalState(emptyPlayerState)
    val s6 = s5.evalState(emptyPlayerState)
    s6.run
  }

  def gameAlongStep(): Option[Player] = {
    type PlayerEffects = Fx.fx4[State[PlayerState[Mike], _], State[PlayerState[Peter], _], 
                                State[PlayerState[Nicole], _], State[PlayerState[Annette], _]]
    type TableEffects = Fx.fx2[State[TableState, _], GameStep]
    val players = aPlayers[PlayerEffects]
    val playerIdentities = players.keys.toSeq
    val game = Game.playGame[TableEffects](playerIdentities, Card.deck.toList)
    val gameOne = game.evalState(emptyTableState(players.keys.toList))
    def loop(stepResult: GameStepResult[Option[Player]], commands: Seq[GameCommand]): Eff[PlayerEffects, Option[Player]] = {
      import GameStepResult._
      stepResult match {
        case GameDone(player) => Eff.pure(player)
        case EventBroadcast(event, continuation) =>
          for {
            commands1 <- players.toSeq.foldM(commands) { (commands, p) =>
              var (player, playerStrategy) = p
              for {
                commands1 <- playerStrategy(player)(event)
              } yield (commands ++ commands1)
            }
            stepResult1 = continuation(())
            next <- loop(stepResult1, commands1)
          } yield next
        case WaitingForCommand(continuation) =>
          if (commands.isEmpty)
            Eff.pure(None)
          else
            loop(continuation(commands.head), commands.tail)
      }
    }
    val p = loop(Game.playOne(gameOne), Seq.empty)
    val p1 = p.evalState(emptyPlayerState)
    val p2 = p1.evalState(emptyPlayerState)
    val p3 = p2.evalState(emptyPlayerState)
    val p4 = p3.evalState(emptyPlayerState)
    p4.run
  }

  def gameAlongIO(): Option[Player] = {
    import GameEvent._
    val mikeIO = PlayerIO.playerIO(mike, aMike[Fx.fx1[State[PlayerState[Mike], _]]])
    val peterIO = PlayerIO.playerIO(peter, aPeter[Fx.fx1[State[PlayerState[Peter], _]]])
    val nicoleIO = PlayerIO.playerIO(nicole, aNicole[Fx.fx1[State[PlayerState[Nicole], _]]])
    val annetteIO = PlayerIO.playerIO(annette, aAnnette[Fx.fx1[State[PlayerState[Annette], _]]])
    val players = List(mike, peter, nicole, annette)
    val game = GameIO.gameIO(players)
    def hasGameEnded(event: GameEvent): Boolean =
      event match {
        case GameEnded(_) => true
        case _ => false
      }
    def loop(commands: Seq[GameCommand]): IO[Option[Player]] =
      if (commands.isEmpty)
        IO.pure(None)
      else
        for {
            events <- commands.traverse(game).map(_.flatten)
            result <-
              events.find(hasGameEnded) match {
                case Some(GameEnded(winner)) =>
                  IO.pure(Some(winner))
                case _ =>
                  for {
                    commands <-
                      events.foldM(Seq.empty[GameCommand]) { (commands, event) =>
                        for {
                          mikeCommands <- mikeIO(event)
                          peterCommands <- peterIO(event)
                          nicoleCommands <- nicoleIO(event)
                          annetteCommands <- annetteIO(event)
                        } yield commands ++ mikeCommands ++ peterCommands ++ nicoleCommands ++ annetteCommands
                      }
                    next <- loop(commands)
                  } yield next
              }
        } yield result
    val hands = players.zip(Game.distribute(players.length, Card.deck.toList).map(_.toSet)).toMap
    import cats.effect.unsafe.implicits.global
    loop(Seq(GameCommand.DealHands(hands))).unsafeRunSync()
  }

  def gameInteractive(): Option[Player] = {
    type PlayerEffects = Fx.prepend[Teletype,
                          Fx.prepend[IO,
                            Fx.fx4[State[PlayerState[Mike], _], State[PlayerState[Peter], _], 
                                    State[PlayerState[Nicole], _], State[PlayerState[Annette], _]
                                    ]]]
    type Effects1 = Fx.prepend[State[TableState, _], PlayerEffects]
    type Effects2 = Fx.prepend[State[Seq[GameCommand], _], Effects1]
    type Effects = Fx.prepend[GameStep, Effects2]
    val players = iPlayers[Effects2]
    val playerIdentities = players.keys.toSeq
    val game = Game.playGame[Effects](playerIdentities, Card.deck.toList)
    val gameWithPlayers = playersGameStep[Effects, Effects2, Option[Player]](players, game)
    val s1 = gameWithPlayers.evalState(Seq.empty)
    val s2 = s1.evalState(emptyTableState(players.keys.toList))
    val s3 = s2.evalState(emptyPlayerState)
    val s4 = s3.evalState(emptyPlayerState)
    val s5 = s4.evalState(emptyPlayerState)
    val s6 = s5.evalState(emptyPlayerState)
    val io = Teletype.runTeletype(s6)
    import cats.effect.unsafe.implicits.global
    io.unsafeRunSync
  }
}