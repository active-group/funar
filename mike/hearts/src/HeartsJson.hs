{-# LANGUAGE OverloadedStrings #-}

module HeartsJson
  ( encodeGameCommand,
    gameCommandDecoder,
    encodeGameEvent,
    gameEventDecoder,
    encodeCard,
    cardDecoder,
    encodePlayer,
    playerDecoder,
    encodeTrick,
    trickDecoder,
    encodeHand,
    handDecoder,
    mapDecoder,
    dataDecoder,
    dealHandsDecoder,
    suitDecoder,
    rankDecoder,
  )
where

import qualified Cards
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GameEvent (GameCommand (..), GameEvent (..))
import Json.Decode (Decoder (..))
import qualified Json.Decode as Decode
import qualified Cards
import Data.Maybe (fromJust)

tuple2Decoder :: Decoder a -> Decoder b -> Decoder (a, b)
tuple2Decoder first second = do
  a <- Decode.index 0 first
  b <- Decode.index 1 second
  pure (a, b)

mapDecoder :: Ord key => Decoder key -> Decoder value -> Decoder (Map.Map key value)
mapDecoder keyDecoder valueDecoder =
  Map.fromList
    <$> Decode.list (tuple2Decoder keyDecoder valueDecoder)

playerDecoder :: Decoder Cards.Player
playerDecoder = constructor1Decoder Decode.string Cards.Player

suitDecoder :: Decoder Cards.Suit
suitDecoder = do
  s <- Decode.string
  case s of
    "Diamonds" -> return Cards.Diamonds
    "Spades" -> return Cards.Spades
    "Hearts" -> return Cards.Hearts
    "Clubs" -> return Cards.Clubs
    s -> Decode.faild ("Not a suit: " <> s)

rankDecoder :: Decoder Cards.Rank
rankDecoder = do
  s <- Decode.string
  case s of
    "Two" -> return Cards.Two
    "Three" -> return Cards.Three
    "Four" -> return Cards.Four
    "Five" -> return Cards.Five
    "Six" -> return Cards.Six
    "Seven" -> return Cards.Seven
    "Eight" -> return Cards.Eight
    "Nine" -> return Cards.Nine
    "Ten" -> return Cards.Ten
    "Jack" -> return Cards.Jack
    "Queen" -> return Cards.Queen
    "King" -> return Cards.King
    "Ace" -> return Cards.Ace
    s -> Decode.faild ("Not a rank: " <> s)

-- >>> runDecoder cardDecoder (fromJust ((Json.decode "{\"rank\":\"Ten\",\"suit\":\"Diamonds\"}") :: Maybe Json.Value))
-- Right (Card {suit = Diamonds, rank = Ten})
cardDecoder :: Decoder Cards.Card
cardDecoder = 
  Cards.Card <$> (Decode.field "suit" suitDecoder) <*> (Decode.field "rank" rankDecoder)

handDecoder :: Decoder Cards.Hand
handDecoder = Cards.makeHand <$> Decode.list cardDecoder

trickDecoder :: Decoder Cards.Trick
trickDecoder = Cards.Trick <$> (Decode.list (tuple2Decoder playerDecoder cardDecoder))

playerHandsDecoder :: Decoder (Map Cards.Player Cards.Hand)
playerHandsDecoder = mapDecoder playerDecoder handDecoder

constructor1Decoder :: Decoder a -> (a -> b) -> Decoder b
constructor1Decoder = flip fmap

constructor2Decoder :: Decoder a -> Decoder b -> (a -> b -> c) -> Decoder c
constructor2Decoder decodeA decodeB constructor =
  uncurry constructor <$> tuple2Decoder decodeA decodeB

handDealtDecoder :: Decoder GameEvent
handDealtDecoder = constructor2Decoder playerDecoder handDecoder HandDealt

playerTurnChangedDecoder :: Decoder GameEvent
playerTurnChangedDecoder = constructor1Decoder playerDecoder PlayerTurnChanged

legalCardPlayedDecoder :: Decoder GameEvent
legalCardPlayedDecoder = constructor2Decoder playerDecoder cardDecoder LegalCardPlayed

trickTakenDecoder :: Decoder GameEvent
trickTakenDecoder = constructor2Decoder playerDecoder trickDecoder TrickTaken

illegalCardPlayedDecoder :: Decoder GameEvent
illegalCardPlayedDecoder = constructor2Decoder playerDecoder cardDecoder IllegalCardAttempted

gameEndedDecoder :: Decoder GameEvent
gameEndedDecoder = constructor1Decoder playerDecoder GameEnded

dealHandsDecoder :: Decoder GameCommand
dealHandsDecoder = constructor1Decoder playerHandsDecoder DealHands

playCardDecoder :: Decoder GameCommand
playCardDecoder = constructor2Decoder playerDecoder cardDecoder PlayCard

dataDecoder :: Map String (Decoder a) -> (String -> Decoder a) -> Decoder a
dataDecoder constructorDecoders defaultDecoder = do
  tag <- Decode.field "tag" Decode.string
  case Map.lookup tag constructorDecoders of
    Nothing -> defaultDecoder tag
    Just decoder -> Decode.field "contents" decoder

gameEventDataTable :: Map String (Decoder GameEvent)
gameEventDataTable =
  Map.fromList
    [ ("HandDealt", handDealtDecoder),
      ("PlayerTurnChanged", playerTurnChangedDecoder),
      ("LegalCardPlayed", legalCardPlayedDecoder),
      ("TrickTaken", trickTakenDecoder),
      ("IllegalCardAttempted", illegalCardPlayedDecoder),
      ("GameEnded", gameEndedDecoder)
    ]

gameEventDecoder :: Decoder GameEvent
gameEventDecoder =
  dataDecoder
    gameEventDataTable
    (\tag -> Decode.faild ("unknown GameEvent tag " ++ tag))

gameEventsDecoder :: Decoder [GameEvent]
gameEventsDecoder = Decode.list gameEventDecoder

gameCommandDecoderTable :: Map String (Decoder GameCommand)
gameCommandDecoderTable =
  Map.fromList
    [ ("PlayCard", playCardDecoder),
      ("DealHands", dealHandsDecoder)
    ]

gameCommandDecoder :: Decoder GameCommand
gameCommandDecoder =
  dataDecoder
    gameCommandDecoderTable
    (\tag -> Decode.faild ("unknown GameCommand tag " ++ tag))

gameCommandsDecoder :: Decoder [GameCommand]
gameCommandsDecoder = Decode.list gameCommandDecoder

jsonString :: String -> Json.Value
jsonString = Json.toJSON . Text.pack

encodePlayer :: Cards.Player -> Json.Value
encodePlayer (Cards.Player playerName) =
  jsonString playerName

encodeSuit :: Cards.Suit -> Json.Value
encodeSuit suit =
  jsonString
    ( case suit of
        Cards.Diamonds -> "Diamonds"
        Cards.Clubs -> "Clubs"
        Cards.Spades -> "Spades"
        Cards.Hearts -> "Hearts"
    )

encodeRank :: Cards.Rank -> Json.Value
encodeRank rank =
  jsonString
    ( case rank of
        Cards.Two -> "Two"
        Cards.Three -> "Three"
        Cards.Four -> "Four"
        Cards.Five -> "Five"
        Cards.Six -> "Six"
        Cards.Seven -> "Seven"
        Cards.Eight -> "Eight"
        Cards.Nine -> "Nine"
        Cards.Ten -> "Ten"
        Cards.Queen -> "Queen"
        Cards.King -> "King"
        Cards.Jack -> "Jack"
        Cards.Ace -> "Ace"
    )

-- >>> Json.encode (encodeCard (Cards.Card Cards.Diamonds Cards.Ten))
-- "{\"rank\":\"Ten\",\"suit\":\"Diamonds\"}"
encodeCard :: Cards.Card -> Json.Value
encodeCard (Cards.Card suit rank) =
  Json.object
    [ "suit" .= encodeSuit suit,
      "rank" .= encodeRank rank
    ]

jsonList :: (a -> Json.Value) -> [a] -> Json.Value
jsonList encode = Json.Array . Vector.fromList . fmap encode

encodeHand :: Cards.Hand -> Json.Value
encodeHand = jsonList encodeCard . Cards.handCards

encodeConstructor2 :: String -> Json.Value -> Json.Value -> Json.Value
encodeConstructor2 constructorName x y =
  Json.object
    [ "tag" .= jsonString constructorName,
      "contents" .= jsonList id [x, y]
    ]

encodeConstructor1 :: String -> Json.Value -> Json.Value
encodeConstructor1 constructorName x =
  Json.object
    [ ("tag", jsonString constructorName),
      ("contents", x)
    ]

encodeGameCommand :: GameCommand -> Json.Value
encodeGameCommand command =
  case command of
    PlayCard player card ->
      encodeConstructor2
        "PlayCard"
        (encodePlayer player)
        (encodeCard card)
    DealHands playerHands ->
      encodeConstructor1
        "DealHands"
        ( jsonList
            ( \(player, hand) ->
                jsonList id [encodePlayer player, encodeHand hand]
            )
            (Map.toList playerHands)
        )

encodeTrick :: Cards.Trick -> Json.Value
encodeTrick trick =
  jsonList
    (\(player, card) -> jsonList id [encodePlayer player, encodeCard card])
    (Cards.trickToList trick)

encodeGameEvent :: GameEvent -> Json.Value
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

encodeGameEvents :: [GameEvent] -> Json.Value
encodeGameEvents = jsonList encodeGameEvent

instance Json.ToJSON GameCommand where
  toJSON = encodeGameCommand

instance Json.FromJSON GameCommand where
  parseJSON json =
    case runDecoder gameCommandDecoder json of
      Left err -> error (show err)
      Right gameCommand -> pure gameCommand

instance Json.ToJSON GameEvent where
  toJSON = encodeGameEvent

instance Json.FromJSON GameEvent where
  parseJSON json =
    case runDecoder gameEventDecoder json of
      Left err -> error (show err)
      Right gameEvent -> pure gameEvent
