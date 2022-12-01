module HeartsJson exposing (..)

import Dict exposing (Dict)

import Json.Decode exposing (Decoder)
import Json.Encode

import HeartsGame exposing (..)

tagDecoder : Decoder String
tagDecoder = Json.Decode.field "tag" Json.Decode.string

playerDecoder : Decoder Player
playerDecoder = constructorDecoder1 Json.Decode.string Player

stringToSuit : String -> Suit
stringToSuit s =
  case s of
    "Diamonds" -> Diamonds
    "Clubs" -> Clubs
    "Spades" -> Spades
    "Hearts" -> Hearts
    _ -> Debug.todo ("unknown suit " ++ s)

suitDecoder : Decoder Suit
suitDecoder = Json.Decode.map stringToSuit Json.Decode.string

stringToRank : String -> Rank
stringToRank s =
  case s of
    "Two" -> Two
    "Three" -> Three
    "Four" -> Four
    "Five" -> Five
    "Six" -> Six
    "Seven" -> Seven
    "Eight" -> Eight
    "Nine" -> Nine
    "Ten" -> Ten

    "Queen" -> Queen
    "King" -> King
    "Jack" -> Jack
    "Ace" -> Ace
    _ -> Debug.todo ("unknown rank tag " ++ s)

rankDecoder : Decoder Rank
rankDecoder =
  Json.Decode.map stringToRank Json.Decode.string

cardDecoder : Decoder Card
cardDecoder =
  Json.Decode.map2 (\ suit rank -> { suit = suit, rank = rank})
                   (Json.Decode.field "suit" suitDecoder)
                   (Json.Decode.field "rank" rankDecoder)

handDecoder : Decoder Hand
handDecoder =
  Json.Decode.list cardDecoder

trickDecoder : Decoder Trick
trickDecoder =
  Json.Decode.list (tuple2Decoder playerDecoder cardDecoder)

playerHandsDecoder : Decoder PlayerHands
playerHandsDecoder = (mapDecoder (Json.Decode.map .name playerDecoder)
                                 (Json.Decode.list cardDecoder))


constructorDecoder1 : Decoder a -> (a -> b) -> Decoder b
constructorDecoder1 field0Decoder constructor =
  Json.Decode.map constructor field0Decoder

constructorDecoder1Nested : Decoder a -> (a -> b) -> Decoder b
constructorDecoder1Nested field0Decoder constructor =
  Json.Decode.map constructor
     (Json.Decode.index 0 field0Decoder)

constructorDecoder2 : Decoder a -> Decoder b -> (a -> b -> c) -> Decoder c
constructorDecoder2 field0Decoder field1Decoder constructor =
    Json.Decode.map2 constructor
     (Json.Decode.index 0 field0Decoder)
     (Json.Decode.index 1 field1Decoder)

handDealtDecoder : Decoder GameEvent
handDealtDecoder =
  constructorDecoder2 playerDecoder handDecoder HandDealt

playerTurnChangedDecoder : Decoder GameEvent
playerTurnChangedDecoder =
  constructorDecoder1 playerDecoder PlayerTurnChanged

legalCardPlayedDecoder : Decoder GameEvent
legalCardPlayedDecoder  =
  constructorDecoder2 playerDecoder cardDecoder LegalCardPlayed

trickTakenDecoder : Decoder GameEvent
trickTakenDecoder =
  constructorDecoder2 playerDecoder trickDecoder TrickTaken

illegalCardPlayedDecoder : Decoder GameEvent
illegalCardPlayedDecoder  =
  constructorDecoder2 playerDecoder cardDecoder IllegalCardAttempted

gameEndedDecoder : Decoder GameEvent
gameEndedDecoder =
  constructorDecoder1 playerDecoder GameEnded

dataDecoder : Dict String (Decoder a) -> (String -> Decoder a) -> Decoder a
dataDecoder constructorDecoders defaultDecoder =
  Json.Decode.field "tag" Json.Decode.string
  |> Json.Decode.andThen (\ tag ->
      case Dict.get tag constructorDecoders of
        Nothing -> defaultDecoder tag
        Just constructorDecoder ->
          Json.Decode.field "contents" constructorDecoder)

gameEventDataTable : Dict String (Decoder GameEvent)
gameEventDataTable =
  Dict.fromList
      [("HandDealt", handDealtDecoder),
       ("PlayerTurnChanged", playerTurnChangedDecoder),
       ("LegalCardPlayed", legalCardPlayedDecoder),
       ("TrickTaken", trickTakenDecoder),
       ("IllegalCardAttempted", illegalCardPlayedDecoder),
       ("GameEnded", gameEndedDecoder)
       ]

gameEventDecoder : Decoder GameEvent
gameEventDecoder =
  dataDecoder
    gameEventDataTable
    (\ tag -> Json.Decode.fail ("unknown GameEvent tag " ++ tag))

gameEventsDecoder : Decoder (List GameEvent)
gameEventsDecoder =
  Json.Decode.list gameEventDecoder

tuple2Decoder : Decoder a -> Decoder b -> Decoder (a, b)
tuple2Decoder aDecoder bDecoder=
  Json.Decode.map2 Tuple.pair
    (Json.Decode.index 0 aDecoder)
    (Json.Decode.index 1 bDecoder)

mapDecoder : Decoder comparable -> Decoder b -> Decoder (Dict comparable b)
mapDecoder keyDecoder valueDecoder =
  Json.Decode.map
    Dict.fromList
    (Json.Decode.list
      (Json.Decode.map2 Tuple.pair
        (Json.Decode.index 0 keyDecoder)
        (Json.Decode.index 1 valueDecoder)))

dealHandsDecoder : Decoder GameCommand
dealHandsDecoder =
  constructorDecoder1 playerHandsDecoder DealHands

playCardDecoder : Decoder GameCommand
playCardDecoder =
  constructorDecoder2 playerDecoder cardDecoder PlayCard

gameCommandDecoderTable =
  Dict.fromList
    [("PlayCard", playCardDecoder)]

gameCommandDecoder : Decoder GameCommand
gameCommandDecoder =
  dataDecoder gameCommandDecoderTable
    (\ tag -> Json.Decode.fail ("unknown GameCommand tag " ++ tag))

gameCommandsDecoder : Decoder (List GameCommand)
gameCommandsDecoder =
  Json.Decode.list gameCommandDecoder

encodeConstructor2 : String -> Json.Encode.Value -> Json.Encode.Value -> Json.Encode.Value
encodeConstructor2 constructorName argA argB =
  Json.Encode.object
    [("tag", Json.Encode.string constructorName),
     ("contents",
        Json.Encode.list identity [argA, argB])]

encodeConstructor1 : String -> Json.Encode.Value -> Json.Encode.Value
encodeConstructor1 constructorName arg =
  Json.Encode.object
    [("tag", Json.Encode.string constructorName),
     ("contents", arg)]

encodeConstructor0 : String -> Json.Encode.Value
encodeConstructor0 constructorName =
  Json.Encode.object
    [("tag", Json.Encode.string constructorName)]

encodePlayer : Player -> Json.Encode.Value
encodePlayer player =
  Json.Encode.string player.name

encodeSuit : Suit -> Json.Encode.Value
encodeSuit suit =
  Json.Encode.string
    (case suit of
      Diamonds -> "Diamonds"
      Clubs -> "Clubs"
      Spades -> "Spades"
      Hearts -> "Hearts")

encodeRank: Rank -> Json.Encode.Value
encodeRank rank =
  Json.Encode.string
    (case rank of
      Two -> "Two"
      Three -> "Three"
      Four -> "Four"
      Five -> "Five"
      Six -> "Six"
      Seven -> "Seven"
      Eight -> "Eight"
      Nine -> "Nine"
      Ten -> "Ten"
      Queen -> "Queen"
      King -> "King"
      Jack  -> "Jack"
      Ace ->  "Ace")

encodeCard : Card -> Json.Encode.Value
encodeCard card  =
  Json.Encode.object
    [("suit", encodeSuit card.suit),
     ("rank", encodeRank card.rank)]

encodeHand : Hand -> Json.Encode.Value
encodeHand hand = Json.Encode.list encodeCard hand

encodeGameCommand : GameCommand -> Json.Encode.Value
encodeGameCommand command =
  case command of
    PlayCard player card ->
      encodeConstructor2 "PlayCard"
        (encodePlayer player) (encodeCard card)
    DealHands playerHands ->
      encodeConstructor1 "DealHands"
        (Json.Encode.list
          (\(name, hand) ->
            let player = Player name
            in Json.Encode.list identity [encodePlayer player, encodeHand hand])
          (Dict.toList playerHands))

encodeTrick : Trick -> Json.Encode.Value
encodeTrick trick =
  Json.Encode.list
    (\ (player, card) -> Json.Encode.list identity [encodePlayer player, encodeCard card])
    trick

encodeGameEvent : GameEvent -> Json.Encode.Value
encodeGameEvent gameEvent =
  case gameEvent of
    PlayerTurnChanged player ->
      encodeConstructor1 "PlayerTurnChanged" (encodePlayer player)
    LegalCardPlayed player card ->
      encodeConstructor2 "LegalCardPlayed" (encodePlayer player) (encodeCard card)
    IllegalCardAttempted player card ->
      encodeConstructor2 "IllegalCardAttempted" (encodePlayer player) (encodeCard card)
    GameEnded player ->
      encodeConstructor1 "GameEnded" (encodePlayer player)
    HandDealt player hand ->
      encodeConstructor2 "HandDealt" (encodePlayer player) (encodeHand hand)
    TrickTaken player trick ->
      encodeConstructor2 "TrickTaken" (encodePlayer player) (encodeTrick trick)
