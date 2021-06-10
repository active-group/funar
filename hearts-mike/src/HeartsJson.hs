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

-- (>>=) :: m a -> (a -> m b) -> m b
tuple2Decoder :: Decoder a -> Decoder b -> Decoder (a, b)
tuple2Decoder first second = do
  a <- Decode.index 0 first
  b <- Decode.index 1 second
  pure (a, b) -- Synonym für return

mapDecoder :: Ord key => Decoder key -> Decoder value -> Decoder (Map.Map key value)
mapDecoder keyDecoder valueDecoder =
  Map.fromList <$> -- fmap
       (Decode.list (tuple2Decoder keyDecoder valueDecoder))

playerDecoder :: Decoder Cards.Player
playerDecoder =
  do playerId <- Decode.field "playerId" Decode.string
     playerName <- Decode.field "playerName" Decode.string 
     return (Cards.Player playerId playerName)
    {-
  Cards.Player
    <$> Decode.field "playerId" Decode.string
    <*> Decode.field "playerName" Decode.string
-}

stringToSuit :: String -> Cards.Suit
stringToSuit name = case name of
  "Diamonds" -> Cards.Diamonds
  "Spades" -> Cards.Spades
  "Hearts" -> Cards.Hearts
  "Clubs" -> Cards.Clubs
  s -> error ("Not a suit: " ++ s)

suitDecoder :: Decoder Cards.Suit
suitDecoder = stringToSuit <$> Decode.string

stringToRank :: String -> Cards.Rank
stringToRank name = case name of
  "Two" -> Cards.Two
  "Three" -> Cards.Three
  "Four" -> Cards.Four
  "Five" -> Cards.Five
  "Six" -> Cards.Six
  "Seven" -> Cards.Seven
  "Eight" -> Cards.Eight
  "Nine" -> Cards.Nine
  "Ten" -> Cards.Ten
  "Jack" -> Cards.Jack
  "Queen" -> Cards.Queen
  "King" -> Cards.King
  "Ace" -> Cards.Ace
  s -> error ("Not a rank: " ++ s)

rankDecoder :: Decoder Cards.Rank
rankDecoder = stringToRank <$> Decode.string

cardDecoder :: Decoder Cards.Card
cardDecoder =
  -- same as:
  -- liftA2 Cards.Card (Decode.field "suit" suitDecoder) (Decode.field "rank" rankDecoder)
  Cards.Card
    <$> Decode.field "suit" suitDecoder
    <*> Decode.field "rank" rankDecoder

-- <$> ::   (a -> b) -> f a -> f b
-- <*> :: f (a -> b) -> f a -> f b

handDecoder :: Decoder Cards.Hand
handDecoder = Set.fromList <$> Decode.list cardDecoder

trickDecoder :: Decoder Cards.Trick
trickDecoder = Decode.list (tuple2Decoder playerDecoder cardDecoder)

playerHandsDecoder :: Decoder Cards.PlayerHands
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
illegalCardPlayedDecoder = constructor2Decoder playerDecoder cardDecoder IllegalCardPlayed

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
      ("IllegalCardPlayed", illegalCardPlayedDecoder),
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
encodePlayer (Cards.Player playerId playerName) =
  Json.object
    [ "playerName" .= jsonString playerName,
      "playerId" .= jsonString playerId
    ]

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

encodeCard :: Cards.Card -> Json.Value
encodeCard (Cards.Card suit rank) =
  Json.object
    [ "suit" .= encodeSuit suit,
      "rank" .= encodeRank rank
    ]

jsonList :: (a -> Json.Value) -> [a] -> Json.Value
jsonList encode = Json.Array . Vector.fromList . fmap encode

encodeHand :: Cards.Hand -> Json.Value
encodeHand = jsonList encodeCard . Set.toList

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
            ( \(Cards.Player playerId _, hand) ->
                jsonList id [encodePlayer (Cards.Player playerId "bonzo"), encodeHand hand]
            )
            (Map.toList playerHands)
        )

encodeTrick :: Cards.Trick -> Json.Value
encodeTrick trick =
  jsonList
    (\(player, card) -> jsonList id [encodePlayer player, encodeCard card])
    trick

encodeGameEvent :: GameEvent -> Json.Value
encodeGameEvent gameEvent =
  case gameEvent of
    PlayerTurnChanged player ->
      encodeConstructor1 "PlayerTurnChanged" (encodePlayer player)
    LegalCardPlayed player card ->
      encodeConstructor2 "LegalCardPlayed" (encodePlayer player) (encodeCard card)
    IllegalCardPlayed player card ->
      encodeConstructor2 "IllegalCardPlayed" (encodePlayer player) (encodeCard card)
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
