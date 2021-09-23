package funar.hearts

import cats._
import cats.implicits._
import cats.data._
import cats.effect.IO
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.Members.{&:, &&:, extractMember}
import org.atnos.eff.syntax.addon.cats.effect._
import org.atnos.eff.addon.cats.effect._
import org.atnos.eff.addon.cats.effect.IOEffect._

object Sync {
  import Player._
  import Table._
  import TableEventSourcing._

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
  type _iEffects[Player, R] = (State[PlayerState[Player], *] |= R) &&: (Teletype |= R)
  
  def iMike[Effects: _iEffects[Mike, *]]: Strategy[Effects] = interactiveStrategy[Mike, Effects]
  def iPeter[Effects: _iEffects[Peter, *]]: Strategy[Effects] = interactiveStrategy[Peter, Effects]
  def iNicole[Effects: _iEffects[Nicole, *]]: Strategy[Effects] = interactiveStrategy[Nicole, Effects]
  def iAnnette[Effects: _iEffects[Annette, *]]: Strategy[Effects] = interactiveStrategy[Annette, Effects]

  def iPlayers[Effects: _iEffects[Mike, *] : _iEffects[Peter, *] : _iEffects[Nicole, *] : _iEffects[Annette, *]] =
    addPlayer(addPlayer(addPlayer(addPlayer(emptyPlayers, mike, iMike[Effects]), 
                                                          peter, iPeter[Effects]), 
                                                          nicole, iNicole[Effects]), 
                                                          annette, iAnnette[Effects])

  type _aEffects[Player, R] = State[PlayerState[Player], *] |= R
  def aMike[Effects: _aEffects[Mike, *]]: Strategy[Effects] = alongStrategy[Mike, Effects]
  def aPeter[Effects: _aEffects[Peter, *]]: Strategy[Effects] = alongStrategy[Peter, Effects]
  def aNicole[Effects: _aEffects[Nicole, *]]: Strategy[Effects] = alongStrategy[Nicole, Effects]
  def aAnnette[Effects: _aEffects[Annette, *]]: Strategy[Effects] = alongStrategy[Annette, Effects]

  def aPlayers[Effects: _aEffects[Mike, *] : _aEffects[Peter, *] : _aEffects[Nicole, *] : _aEffects[Annette, *]] =
    addPlayer(addPlayer(addPlayer(addPlayer(emptyPlayers, mike, aMike[Effects]), 
                                                          peter, aPeter[Effects]), 
                                                          nicole, aNicole[Effects]), 
                                                          annette, aAnnette[Effects])

  def playEvent[Effects](players: Players[Effects], event: GameEvent): Eff[Effects, Seq[GameCommand]] =
    players.toList.foldM[Eff[Effects, *], Seq[GameCommand]](Seq.empty) { (gameCommands: Seq[GameCommand], pair: (Player, Strategy[Effects])) =>
      val (player, playerStrategy) = pair
      for {
        gameCommands1 <- playerStrategy(player)(event)
      } yield (gameCommands ++ gameCommands1)
    }

  type GameEventSourcing = Fx.fx2[State[TableState, *], EventSourcing[GameEvent, *]]

  def foo[R](effects: Eff[R, Unit]): Eff[Fx.prepend[State[Int, *], R], Unit] =
    effects.into[Fx.prepend[State[Int, *], R]]

  def playCommand[Effects : _tableState : _gameEventSourcing](players: Players[Effects], command: GameCommand): Eff[Effects, Unit] =
    for {
      events <- gameCommandEventsM[Effects](command)
      maybeWinner <- gameOverM[Effects]
      _ <- maybeWinner match {
             case Some(_) => Eff.pure[Effects, Unit](())
             case None =>
              for {
                gameCommandss <- events.traverse(playEvent[Effects](players, _))
                gameCommands = gameCommandss.flatten
                _ <- gameCommands.traverseTap(playCommand(players, _))
              } yield ()
           }
    } yield ()

  def playGame[Effects : _tableState : _gameEventSourcing](players: Players[Effects], cards: Seq[Card]): Eff[Effects, Unit] = {
    val playersSeq = players.keys
    val hands = Map.from(players.keys.zip(distribute(playersSeq.size, cards.toList).map(_.toSet)))
    playCommand(players, GameCommand.DealHands(hands))
  }

  def gameAlong: Seq[GameEvent] = {
    // Fx.fx6 diverges, as does Fx.append[Fx2, Fx4]
    type PlayerEffects = Fx.fx4[State[PlayerState[Mike], *], State[PlayerState[Peter], *], 
                                State[PlayerState[Nicole], *], State[PlayerState[Annette], *]]
    type Effects1 = Fx.prepend[State[Seq[GameEvent], *], PlayerEffects]
    type Effects2 = Fx.prepend[EventSourcing[GameEvent, *], Effects1]
    type Effects = Fx.prepend[State[TableState, *], Effects2]
    
    val players = aPlayers[Effects]
    val game = playGame[Effects](players, Card.deck)
    val e1: Eff[Effects2, Unit] = 
          game.evalState(emptyTableState(players.keys.toList))  
    val e2: Eff[Effects1, Unit] = EventSourcing.runEventSourcingState[GameEvent, Effects2, Effects1, Unit](e1)
    val e3: Eff[PlayerEffects, Seq[GameEvent]] = e2.execState[Seq[GameEvent]](Seq.empty)
    val e4 = e3.evalState[PlayerState[Mike]](emptyPlayerState)
    val e5 = e4.evalState[PlayerState[Peter]](emptyPlayerState)
    val e6 = e5.evalState[PlayerState[Nicole]](emptyPlayerState)
    val e7 = e6.evalState[PlayerState[Annette]](emptyPlayerState)
    e7.run
  }

  def gameInteractive = {
    type PlayerEffects = Fx.prepend[Teletype,
                          Fx.prepend[IO,
                            Fx.fx4[State[PlayerState[Mike], *], State[PlayerState[Peter], *], 
                                    State[PlayerState[Nicole], *], State[PlayerState[Annette], *]
                                    ]]]
    type Effects1 = Fx.prepend[State[Seq[GameEvent], *], PlayerEffects]
    type Effects2 = Fx.prepend[EventSourcing[GameEvent, *], Effects1]
    type Effects = Fx.prepend[State[TableState, *], Effects2]
    
    val players = iPlayers[Effects]
    val game = playGame[Effects](players, Card.deck)
    val e1: Eff[Effects2, Unit] = 
           game.evalState(emptyTableState(players.keys.toList))  
    val e2: Eff[Effects1, Unit] = EventSourcing.runEventSourcingState[GameEvent, Effects2, Effects1, Unit](e1)
    val e3: Eff[PlayerEffects, Seq[GameEvent]] = e2.execState[Seq[GameEvent]](Seq.empty)
    val e4 = e3.evalState[PlayerState[Mike]](emptyPlayerState)
    val e5 = e4.evalState[PlayerState[Peter]](emptyPlayerState)
    val e6 = e5.evalState[PlayerState[Nicole]](emptyPlayerState)
    val e7 = e6.evalState[PlayerState[Annette]](emptyPlayerState)
    val e8: Eff[Fx.fx1[IO], Seq[GameEvent]] = Teletype.runTeletype[Fx.fx2[Teletype, IO], Fx.fx1[IO], Seq[GameEvent]](e7)
    e8.unsafeRunSync
  }


  def distribute[A](nPiles: Int, things: List[A]): Seq[List[A]] = {
    def extract(s: List[A]): List[A] =
      s match {
        case Nil => Nil
        case x::xs => x :: extract(xs.drop(nPiles-1))
      }
    for {
      i <- 0 until nPiles
    } yield extract(things.drop(i))
  }


}