package funar.hearts

import cats.{Functor, Monad, Traverse}
import cats.data.*
import cats.implicits.*
import cats.effect.IO
import funar.hearts.Rank.Two
import funar.hearts.Suit.Clubs

object Strategy {
  import GameEvent._
  import Cards._
  import Free._

  type Pile = Set[Card]
  case class PlayerState(player: Player, hand: Hand, trick: Trick, pile: Pile)

  def makeEmptyPlayerState(player: Player): PlayerState =
    PlayerState(player, Hand.empty, Trick.empty, Set.empty)

  def playerProcessEvent(event: GameEvent, state: PlayerState): PlayerState =
    event match {
      case HandDealt(player1, hand) =>
        if (state.player == player1)
          state.copy(hand = hand, trick = Trick.empty, pile = Set.empty)
        else
          state
      case PlayerTurnChanged(player1) => state
      case LegalCardPlayed(player1, card) =>
        if (state.player == player1)
          state.copy(hand = Hand.removeCard(state.hand, card),
                     trick = Trick.add(state.trick, player1, card))
        else
          state.copy(trick = Trick.add(state.trick, player1, card))
      case IllegalCardAttempted(player1, card) => state
      case TrickTaken(player1, trick) =>
        if (player1 == state.player)
          state.copy(trick = Trick.empty,
                     pile = state.pile ++ Trick.cards(trick))
        else
          state.copy(trick = Trick.empty)
      case GameEnded(winner) => state
    }

  enum StatePlayerF[R] {
    case GetEvent[R](cont: GameEvent => R) extends StatePlayerF[R]
    case RecordCommand[R](command: GameCommand, cont: Unit => R) extends StatePlayerF[R]
    case GetPlayerState(cont: PlayerState => R) extends StatePlayerF[R]
  }

  given statePlayerFunctor : Functor[StatePlayerF] with {
    import StatePlayerF._
    override def map[A, B](fa: StatePlayerF[A])(f: A => B): StatePlayerF[B] =
      fa match {
        case GetEvent(cont) => GetEvent({ event => f(cont(event))})
        case RecordCommand(command, cont) =>
          RecordCommand(command, { _ => f(cont(()))})
        case GetPlayerState(cont) =>
          GetPlayerState({ state => f(cont(state)) })
      }
  }

  type StatePlayer[A] = Free[StatePlayerF, A]

  trait StatePlayerMonad[M[_]] {
    def getEventM : M[GameEvent]
    def recordCommandM(command: GameCommand): M[Unit]
    def getPlayerStateM : M[PlayerState]
  }

  given statePlayerMonad : StatePlayerMonad[StatePlayer] with {
    import StatePlayerF._
    override def getEventM = Impure(GetEvent(Pure(_)))
    override def recordCommandM(command: GameCommand) = Impure(RecordCommand(command, Pure(_)))
    override def getPlayerStateM = Impure(GetPlayerState(Pure(_)))
  }

  def runStatePlayer[A](playerM: StatePlayer[A], state: PlayerState, commands: Seq[GameCommand])
    : (Seq[GameCommand], Either[GameEvent => (PlayerState, StatePlayer[A]), A]) = {
    import StatePlayerF._
    playerM match {
      case Pure(result) => (commands, Right(result))
      case Impure(GetEvent(cont)) =>
        (commands, Left({ (event: GameEvent) => (playerProcessEvent(event, state), cont(event)) }))
      case Impure(RecordCommand(command, cont)) =>
        runStatePlayer(cont(()), state, commands.appended(command))
      case Impure(GetPlayerState(cont)) =>
        runStatePlayer(cont(state), state, commands)
    }
  }

  def statePlayerIO[A](player: Player, playerM: StatePlayer[A])
    : GameEvent => IO[Seq[GameCommand]] = {
    val emptyPlayerState = makeEmptyPlayerState(player)
    val (commands0, step0) = runStatePlayer(playerM, emptyPlayerState, Seq.empty)
    var ref = (emptyPlayerState, commands0, step0)
    def processEvent(event: GameEvent): IO[Seq[GameCommand]] = {
      val (state, commands, step) = ref
      step match {
        case Left(cont) => IO {
          val (state1, playerM1) = cont(event)
          val (commands1, step1) = runStatePlayer(playerM1, state1, Seq.empty)
          ref = (state1, Seq.empty, step1)
          commands ++ commands1
        }
        case Right(_result) => IO.pure(commands)
      }
    }
    processEvent
  }

  def chooserStrategy[M[_]: StatePlayerMonad : Monad](player: Player, chooseM: M[Card]): M[Unit] = {
    val sm = summon[StatePlayerMonad[M]]
    import sm.*
    val m = summon[Monad[M]]
    import m.*
    import GameEvent._
    import GameCommand._
    getEventM >>= { event =>
      event match {
        case HandDealt(player1, hand) =>
          if ((player == player1) && Hand.containsCard(hand, Card(Clubs, Two)))
            recordCommandM(PlayCard(player, Card(Clubs, Two)))
          else
            pure(())
        case PlayerTurnChanged(player1) =>
          if (player == player1)
            chooseM >>= { card => recordCommandM(PlayCard(player, card)) }
          else
            pure(())
        case LegalCardPlayed(player1, card) => pure(())
        case IllegalCardAttempted(player1, card) => pure(())
        case TrickTaken(player1, trick) => pure(())
        case GameEnded(winner) => pure(())
      }
    } >>= { _ => chooserStrategy(player, chooseM) }
  }

  def chooseAlong[M[_] : StatePlayerMonad : Monad]: M[Card] = {
    val sm = summon[StatePlayerMonad[M]]
    import sm.*
    val m = summon[Monad[M]]
    import m.*
    getPlayerStateM >>= { state =>
      val cards = Hand.cards(state.hand)
      // kleine erste Karte
      if (Trick.isEmpty(state.trick))
        pure(Hand.cards(state.hand).minBy(_.rank))
      else {
        val firstCard = Trick.leadingCard(state.trick)
        val followingCardsOnHand = Hand.cards(state.hand).filter {
          _.suit == firstCard.suit
        }
        followingCardsOnHand.minByOption(_.rank) match {
          case None => pure(Hand.cards(state.hand).maxBy(_.rank))
          case Some(card) => pure(card)
        }
      }
    }
  }

  def alongStrategy[M[_] : StatePlayerMonad : Monad](player: Player): M[Unit] =
    chooserStrategy(player, chooseAlong)

  enum StateTtyPlayerF[R] {
    case GetEvent[R](cont: GameEvent => R) extends StateTtyPlayerF[R]
    case RecordCommand[R](command: GameCommand, cont: Unit => R) extends StateTtyPlayerF[R]
    case GetPlayerState(cont: PlayerState => R) extends StateTtyPlayerF[R]
    case ReadLine(cont: String => R) extends StateTtyPlayerF[R]
    case WriteLine(line: String, cont: Unit => R) extends StateTtyPlayerF[R]
  }

  given stateTtyPlayerFunctor: Functor[StateTtyPlayerF] with {
    import StateTtyPlayerF._

    override def map[A, B](fa: StateTtyPlayerF[A])(f: A => B): StateTtyPlayerF[B] =
      fa match {
        case GetEvent(cont) => GetEvent({ event => f(cont(event)) })
        case RecordCommand(command, cont) =>
          RecordCommand(command, { _ => f(cont(())) })
        case GetPlayerState(cont) =>
          GetPlayerState({ state => f(cont(state)) })
        case ReadLine(cont) => ReadLine({ line => f(cont(line))} )
        case WriteLine(line, cont) => WriteLine(line, { _ => f(cont(()))})
      }
  }

  type StateTtyPlayer[A] = Free[StateTtyPlayerF, A]

  given stateTtyPlayerMonad: StatePlayerMonad[StateTtyPlayer] with {
    import StateTtyPlayerF._
    override def getEventM = Impure(GetEvent(Pure(_)))
    override def recordCommandM(command: GameCommand) = Impure(RecordCommand(command, Pure(_)))
    override def getPlayerStateM = Impure(GetPlayerState(Pure(_)))
  }

  trait TtyPlayerMonad[M[_]] {
    def readLineM : M[String]
    def writeLineM(line: String): M[Unit]
  }

  given ttyPlayerMonad: TtyPlayerMonad[StateTtyPlayer] with {
    import StateTtyPlayerF._
    def readLineM = Impure(ReadLine(Pure(_)))
    def writeLineM(line: String) = Impure(WriteLine(line, Pure(_)))
  }

  def chooseInteractive[M[_] : TtyPlayerMonad : StatePlayerMonad : Monad]: M[Card] = {
    val tm = summon[TtyPlayerMonad[M]]
    import tm._
    val sm = summon[StatePlayerMonad[M]]
    import sm._
    val m = summon[Monad[M]]
    import m.*
    for {
        state <- sm.getPlayerStateM
        _ <- writeLineM("Your turn, player " ++ state.player.name)
        handList = Hand.cards(state.hand).toList
        _ <- if (Trick.isEmpty(state.trick))
          writeLineM("You lead the next trick.")
        else
          writeLineM("Trick: " ++ Trick.pretty(state.trick))
        _ <- writeLineM("Your hand:\n" ++ prettyCards(handList))
        ncards = handList.size
        _ <- writeLineM("Pick a card (1-" ++ ncards.toString ++ ")")
        selected <- getNumber(1, ncards)
      } yield handList(selected - 1)
  }

  def getNumber[M[_] : TtyPlayerMonad : Monad](lo: Int, hi: Int): M[Int] = {
    val tm = summon[TtyPlayerMonad[M]]
    import tm._
    val m = summon[Monad[M]]
    import m.*
    for {
      s <- readLineM
      input = s.toInt
      n <- if ((lo <= input) && (input <= hi))
        pure(input)
      else
        for {
          _ <- writeLineM("Input must be between " ++ lo.toString ++ " and " ++ hi.toString ++ ". Try again.")
          n <- getNumber(lo, hi)
        } yield n
    } yield n
  }

  def prettyCards(cards: List[Card]): String = {
    def prettyOne(n: Integer, card: Card): String = "(" ++ n.toString ++ ")" ++ Trick.prettyCard(card)

    def loop(n: Integer, cards: List[Card]): String =
      cards match {
        case Nil => ""
        case List(card) => prettyOne(n, card)
        case card :: cards => prettyOne(n, card) ++ "\n" ++ loop(n + 1, cards)
      }

    loop(1, cards)
  }

  enum StateTtyPlayerStep[A] {
    case WaitingForEvent(cont: GameEvent => (PlayerState, StateTtyPlayer[A]))
    case WaitingForLine(cont: String => StateTtyPlayer[A])
    case StepDone(result: A)
  }

  def runStateTtyPlayer[A](playerM: StateTtyPlayer[A], state: PlayerState,
                           commands: Seq[GameCommand], lines: Seq[String])
    : (Seq[GameCommand], Seq[String], StateTtyPlayerStep[A]) = {
    import StateTtyPlayerStep._
    import StateTtyPlayerF._
    playerM match {
      case Pure(result) => (commands, lines, StepDone(result))
      case Impure(GetEvent(cont)) =>
        (commands, lines,
          WaitingForEvent({ event => (playerProcessEvent(event, state), cont(event)) }))
      case Impure(RecordCommand(command, cont)) =>
        runStateTtyPlayer(cont(()), state, commands.appended(command), lines)
      case Impure(GetPlayerState(cont)) =>
        runStateTtyPlayer(cont(state), state, commands, lines)
      case Impure(ReadLine(cont)) =>
        (commands, lines, WaitingForLine(cont))
      case Impure(WriteLine(line, cont)) =>
        runStateTtyPlayer(cont(()), state, commands, lines.appended(line))
    }
  }

  def stateTtyPlayerIO[A](player: Player, playerM: StateTtyPlayer[A])
  : GameEvent => IO[Seq[GameCommand]] = {
    import StateTtyPlayerStep._
    val emptyPlayerState = makeEmptyPlayerState(player)
    val (commands0, lines0, step0) = runStateTtyPlayer(playerM, emptyPlayerState, Seq.empty, Seq.empty)
    lines0.foreach(println(_))
    var ref = (emptyPlayerState, commands0, step0)
    def handleWaitingForLine: IO[Unit] =
      IO { ref } >>= { case (state, commands, step) =>
        step match {
          case WaitingForLine(cont) => {
            for {
              line <- IO.readLine
              playerM1 = cont(line)
              (commands1, lines1, step1) = runStateTtyPlayer(playerM1, state, Seq.empty, Seq.empty)
              _ <- lines1.map(IO.println).sequence_
              _ <- IO {
                ref = (state, commands ++ commands1, step1)
              }
              _ <- handleWaitingForLine
            } yield ()
          }
          case _ => IO.pure(())
        }
      }
    def processEvent(event: GameEvent): IO[Seq[GameCommand]] =
      IO { ref } >>= { case (state, commands, step) =>
        step match {
          case WaitingForEvent(cont) => {
            val (state1, playerM1) = cont(event)
            val (commands1, lines1, step1) = runStateTtyPlayer(playerM1, state1, Seq.empty, Seq.empty)
            lines1.map(IO.println).sequence_
            >> IO { ref = (state1, commands ++ commands1, step1) }
            >> handleWaitingForLine
            >> IO { ref }
            >>= { case (state, commands, step) =>
              IO { ref = (state, Seq.empty, step)}
              >> IO.pure(commands)
            }
          }
          case WaitingForLine(cont) => IO { throw new Exception("this can't happen") }
          case StepDone(_result) => IO.pure(commands)
        }
      }
    processEvent
  }

  def interactiveStrategy[M[_] : StatePlayerMonad : TtyPlayerMonad : Monad](player: Player): M[Unit] =
    chooserStrategy(player, chooseInteractive)

}

