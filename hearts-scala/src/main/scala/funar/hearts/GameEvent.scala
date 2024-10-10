package funar.hearts

import cats._
import cats.implicits._

import Cards._

enum GameEvent {
	case HandDealt(player: Player, hand: Hand)
	case PlayerTurnChanged(player: Player)
	case LegalCardPlayed(player: Player, card: Card)
	case IllegalCardAttempted(player: Player, card: Card)
	case TrickTaken(player: Player, trick: Trick)
	case GameEnded(won: Player)
}

object GameEvent {
	def eventsWinner(events: Iterable[GameEvent]): Option[Player] =
		events.find({ case GameEnded(won) => true
									case _ => false }) match {
										case Some(GameEnded(won)) => Some(won)
										case _ => None
									}
}

enum GameCommand {
	case DealHands(hands: Map[Player, Hand])
	case PlayCard(player: Player, card: Card)
}

object GameM {
	enum Game[A] {
		case PlayValid(player: Player, card: Card, cont: Boolean => Game[A])
		case RecordEvent(event: GameEvent, cont: Unit => Game[A])
		case GetCommand(cont: GameCommand => Game[A])
		case TurnOverTrick(cont: Option[(Trick, Player)] => Game[A])
		case PlayerAfter(player: Player, cont: Player => Game[A])
		case GameOver(cont: Option[Player] => Game[A])
		case Done(result: A)
	}
	import Game._

	def playValidM(player: Player, card: Card) = PlayValid(player, card, Done(_))
	def recordEventM(event: GameEvent) = RecordEvent(event, Done(_))
	def turnOverTrickM = TurnOverTrick(Done(_))
	def playerAfterM(player: Player) = PlayerAfter(player, Done(_))
	def gameOverM = GameOver(Done(_))
}

import GameM.Game

given gameMonad : Monad[GameM.Game[_]] with {
	override def pure[A](result: A): Game[A] = Game.Done(result)
	override def flatMap[A, B](game: Game[A])(next: A => Game[B]): Game[B] = {
		import Game._
		game match {
			case PlayValid(player, card, cont) =>
				PlayValid(player, card, valid => cont(valid).flatMap(next))
			case RecordEvent(event, cont) =>
				RecordEvent(event, _ => cont(()).flatMap(next))
			case GetCommand(cont) =>
				GetCommand(command => cont(command).flatMap(next))
			case TurnOverTrick(cont) =>
				TurnOverTrick(over => cont(over).flatMap(next))
			case PlayerAfter(player, cont) =>
				PlayerAfter(player, player => cont(player).flatMap(next))
			case GameOver(cont) =>
				GameOver(won => cont(won).flatMap(next))
			case Done(result) => next(result)
		}
	}
	override def tailRecM[A, B](a: A)(f: A => Game[Either[A, B]]): Game[B] = {
		
		import Game._
		def loop(a: A): Game[B] =
			f(a).flatMap {
				case Left(a1) => tailRecM(a1)(f)
				case Right(b) => Done(b)
			}
		loop(a)
	}
}

def tableProcessCommandM(command: GameCommand): Game[Option[Player]] = {
	import GameCommand._
	import GameEvent._
	import GameM._
	import gameMonad._
	command match {
		case DealHands(hands) =>
			val records =
				hands.map { case (player, hand) => recordEventM(HandDealt(player, hand)) }.toSeq
			Foldable[Seq].sequence_(records).map { _ => None }
		case PlayCard(player, card) =>
			playValidM(player, card) >>= { valid =>
				if (valid) {
					recordEventM(LegalCardPlayed(player, card)) >>
					turnOverTrickM >>= { turnOverTrick =>
						turnOverTrick match {
							case Some((trick, trickTaker)) =>
								recordEventM(TrickTaken(trickTaker, trick)) >>
								gameOverM >>= { gameOver =>
									gameOver match {
										case Some(winner) =>
											for {
												_ <- recordEventM(GameEnded(winner))
											} yield Some(winner)
										case None =>
											recordEventM(PlayerTurnChanged(trickTaker)) >> pure(None)
									}
								}
							case None =>
								playerAfterM(player) >>= { nextPlayer =>
									recordEventM(PlayerTurnChanged(nextPlayer)).map { _ => None }
								}
						}
					}
				} else 
					recordEventM(IllegalCardAttempted(player, card)) >> pure(None)
			}
		}
	}

def tableLoopM(command: GameCommand): Game[Option[Player]] =
	tableProcessCommandM(command) >>= {
		case None => Game.GetCommand(tableLoopM)
		case Some(winner) =>
			Game.Done(Some(winner))
	}
