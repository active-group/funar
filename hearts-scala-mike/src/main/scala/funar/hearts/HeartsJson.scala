package funar.hearts


import funar.json.Decode._
import io.circe
import io.circe.Json
import cats._
import cats.implicits._
import cats.data._
import scala.collection.mutable

object HeartsJson {
  import GameEvent._
  import GameCommand._

  val tagDecoder: Decoder[String] = field("tag", string)

  val playerDecoder: Decoder[Player] =
    Applicative[Decoder].map2(field("playerName", string), field("playerId", string)) { (name, id) =>
      Player(name = name, id = id)
    }

  def stringToSuit(s: String): Suit =
    s match {
      case "Diamonds" => Suit.Diamonds
      case "Clubs" => Suit.Clubs
      case "Spades" => Suit.Spades
      case "Hearts" => Suit.Hearts
      case _ => require(false, "unknown suit " + s); ???
    }
  
    val suitDecoder: Decoder[Suit] = string.map(stringToSuit)

    def stringToRank(s: String): Rank = {
      import Rank._
      s match {
        case "Two" => Two
        case "Three" => Three
        case "Four" => Four
        case "Five" => Five
        case "Six" => Six
        case "Seven" => Seven
        case "Eight" => Eight
        case "Nine" => Nine
        case "Ten" => Ten

        case "Queen" => Queen
        case "King" => King
        case "Jack" => Jack
        case "Ace" => Ace
        case _ => require(false, "unknown rank tag " ++ s); ???
      }
    }

  val rankDecoder: Decoder[Rank] = string.map(stringToRank)

  val cardDecoder: Decoder[Card] =
    Applicative[Decoder].map2(field("suit", suitDecoder), field("rank", rankDecoder))(Card(_, _))

  val handDecoder: Decoder[Hand] =
    list(cardDecoder).map(_.toSet)

  val trickDecoder: Decoder[Trick] =
    list(tuple2Decoder(playerDecoder, cardDecoder))

  def constructorDecoder1[A, B](field0Decoder: Decoder[A], constructor: A => B): Decoder[B] =
    field0Decoder.map(constructor)

  def constructorDecoder1Nested[A, B](field0Decoder: Decoder[A], constructor: A => B): Decoder[B] =
    index(0, field0Decoder).map(constructor)

  def constructorDecoder2[A, B, C](field0Decoder: Decoder[A], field1Decoder: Decoder[B], constructor: (A, B) => C)
      : Decoder[C] =
    Applicative[Decoder].map2(index(0, field0Decoder), index(1, field1Decoder))(constructor)

  def handDealtDecoder: Decoder[GameEvent] =
    constructorDecoder2(playerDecoder, handDecoder, HandDealt)

  def playerTurnChangedDecoder: Decoder[GameEvent] =
    constructorDecoder1(playerDecoder, PlayerTurnChanged)

  def legalCardPlayedDecoder: Decoder[GameEvent] =
    constructorDecoder2(playerDecoder, cardDecoder, LegalCardPlayed)

  def trickTakenDecoder: Decoder[GameEvent] =
    constructorDecoder2(playerDecoder, trickDecoder, TrickTaken)

  def illegalCardPlayedDecoder: Decoder[GameEvent] =
    constructorDecoder2(playerDecoder, cardDecoder, IllegalCardPlayed)

  def gameEndedDecoder: Decoder[GameEvent] =
    constructorDecoder1(playerDecoder, GameEnded)

  def dataDecoder[A](constructorDecoders: Map[String, Decoder[A]], defaultDecoder: String => Decoder[A]): Decoder[A] =
    for {
      tag <- field("tag", string)
      res <- constructorDecoders.get(tag) match {
              case None => defaultDecoder(tag)
              case Some(constructorDecoder) =>
                field("contents", constructorDecoder)
            }
    } yield res

  val gameEventDataTable: Map[String, Decoder[GameEvent]] =
    Map(("HandDealt", handDealtDecoder),
        ("PlayerTurnChanged", playerTurnChangedDecoder),
        ("LegalCardPlayed", legalCardPlayedDecoder),
        ("TrickTaken", trickTakenDecoder),
        ("IllegalCardPlayed", illegalCardPlayedDecoder),
        ("GameEnded", gameEndedDecoder))

  val gameEventDecoder: Decoder[GameEvent] =
    dataDecoder(gameEventDataTable, tag => fail("unknown GameEvent tag " + tag))

  val gameEventsDecoder: Decoder[List[GameEvent]] = list(gameEventDecoder)

  def tuple2Decoder[A, B](decoderA: Decoder[A], decoderB: Decoder[B]): Decoder[(A, B)] =
    Applicative[Decoder].map2(index(0, decoderA), index(1, decoderB))((_, _))

  def mapDecoder[Key, Value](keyDecoder: Decoder[Key], valueDecoder: Decoder[Value]): Decoder[Map[Key, Value]] =
    list(tuple2Decoder(keyDecoder, valueDecoder)).map(Map.from)

  val dealHandsDecoder: Decoder[GameCommand] =
    constructorDecoder1(mapDecoder(playerDecoder, list(cardDecoder).map(_.toSet)), DealHands(_))

  val playCardDecoder: Decoder[GameCommand] =
    constructorDecoder2(playerDecoder, cardDecoder, PlayCard)

  val gameCommandDecoderTable =
    Map("PlayCard" -> playCardDecoder,
        "DealHands" -> dealHandsDecoder)

  val gameCommandDecoder: Decoder[GameCommand] =
    dataDecoder(gameCommandDecoderTable, tag => fail("unknown GameCommand tag " + tag))
    
  val gameCommandsDecoder: Decoder[List[GameCommand]] = list(gameCommandDecoder)

  def encodeConstructor2(constructorName: String, argA: Json, argB: Json): Json =
    Json.obj("tag" -> Json.fromString(constructorName),
             "contents" -> Json.arr(argA, argB))

  def encodeConstructor1(constructorName: String, arg: Json): Json =
    Json.obj("tag" -> Json.fromString(constructorName),
             "contents" -> arg)
                                        
  def encodeConstructor0(constructorName: String): Json =
    Json.obj("tag" -> Json.fromString(constructorName))

  def encodePlayer(player: Player): Json =
    Json.obj("playerName" -> Json.fromString(player.name),
             "playerId" -> Json.fromString(player.id))

  def encodeSuit(suit: Suit): Json = {
    import Suit._
    Json.fromString(suit match {
                      case Diamonds => "Diamonds"
                      case Clubs => "Clubs"
                      case Spades => "Spades"
                      case Hearts => "Hearts"    
                      })
  }

  def encodeRank(rank: Rank): Json = {
    import Rank._
    Json.fromString(rank match {
                      case Two => "Two"
                      case Three => "Three"
                      case Four => "Four"
                      case Five => "Five"
                      case Six => "Six"
                      case Seven => "Seven"
                      case Eight => "Eight"
                      case Nine => "Nine"
                      case Ten => "Ten"
                      case Queen => "Queen"
                      case King => "King"
                      case Jack => "Jack"
                      case Ace =>  "Ace"
                    })
  }

  def encodeCard(card: Card): Json =
    Json.obj("suit" -> encodeSuit(card.suit),
             "rank" -> encodeRank(card.rank))
                                    

  def encodeHand(hand: Hand): Json = Json.arr(hand.map(encodeCard).toIndexedSeq:_*)
    
  def encodeGameCommand(command: GameCommand): Json =
    command match {
      case PlayCard(player, card) => 
        encodeConstructor2("PlayCard", encodePlayer(player), encodeCard(card))
      case DealHands(playerHands) =>
        encodeConstructor1("DealHands",
          Json.arr(playerHands.toIndexedSeq.map { case (player, hand) => 
                                                  Json.arr(encodePlayer(Player(player.id, player.name)), encodeHand(hand))
                                                }:_*))
    }

  def encodeGameCommands(commands: Seq[GameCommand]): Json =
    Json.arr(commands.toIndexedSeq.map(encodeGameCommand):_*)
    
  def encodeTrick(trick: Trick): Json =
    Json.arr(trick.toIndexedSeq.map { case (player, card) =>
                                        Json.arr(encodePlayer(player), encodeCard(card))
                                    }:_*)

  def encodeGameEvent(gameEvent: GameEvent): Json =
    gameEvent match {
      case PlayerTurnChanged(player) =>
        encodeConstructor1("PlayerTurnChanged", encodePlayer(player))
      case LegalCardPlayed(player, card) =>
        encodeConstructor2("LegalCardPlayed", encodePlayer(player), encodeCard (card))
      case IllegalCardPlayed(player, card) =>
        encodeConstructor2("IllegalCardPlayed", encodePlayer(player), encodeCard(card))
      case GameEnded(player) =>
        encodeConstructor1("GameEnded", encodePlayer(player))
      case HandDealt(player, hand) =>
        encodeConstructor2("HandDealt", encodePlayer(player), encodeHand(hand))
      case TrickTaken(player, trick) =>
        encodeConstructor2("TrickTaken", encodePlayer(player), encodeTrick(trick))
    }

  def encodeGameEvents(events: Seq[GameEvent]): Json =
    Json.arr(events.toIndexedSeq.map(encodeGameEvent):_*)
}

