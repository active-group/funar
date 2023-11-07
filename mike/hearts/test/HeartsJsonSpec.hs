{-# LANGUAGE OverloadedStrings #-}

module HeartsJsonSpec (spec) where

import Cards
import qualified Data.Aeson as Json
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators (mapLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GameEvent
import HeartsJson
import Json.Decode (DecodeError (..), Decoder (..))
import qualified Json.Decode as Decode
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Suit where
  arbitrary = oneof $ map pure allSuits

instance Arbitrary Rank where
  arbitrary = oneof $ map pure allRanks

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Player where
  arbitrary = Player <$> arbitrary

instance Arbitrary Hand where
  arbitrary = makeHand <$> arbitrary

instance Arbitrary Trick where
  arbitrary = Trick <$> arbitrary

instance Arbitrary GameEvent where
  arbitrary =
    oneof
      [ HandDealt <$> arbitrary <*> arbitrary,
        PlayerTurnChanged <$> arbitrary,
        LegalCardPlayed <$> arbitrary <*> arbitrary,
        TrickTaken <$> arbitrary <*> arbitrary,
        IllegalCardAttempted <$> arbitrary <*> arbitrary,
        GameEnded <$> arbitrary
      ]

instance Arbitrary GameCommand where
  arbitrary =
    oneof
      [ DealHands <$> arbitrary,
        PlayCard <$> arbitrary <*> arbitrary
      ]

decode :: Decoder a -> ByteString -> Either DecodeError a
decode decoder bs =
  case Json.eitherDecode bs of
    Left err -> error $ "Invalid JSON: " <> err
    Right json -> runDecoder decoder json

jsonRoundtrip :: Eq a => Decoder a -> (a -> Json.Value) -> a -> Bool
jsonRoundtrip decoder encode v =
  runDecoder decoder (encode v) == Right v

spec :: Spec
spec = do
  mapDecoderSpec
  dataDecoderSpec
  dealHandsDecoderSpec
  gameCommandDecoderSpec
  describe "decode . encode ~= identity" $ do
    it "Card" $
      property (jsonRoundtrip cardDecoder encodeCard)
    it "Player" $
      property (jsonRoundtrip playerDecoder encodePlayer)
    it "Hand" $
      property (jsonRoundtrip handDecoder encodeHand)
    it "Trick" $
      property (jsonRoundtrip trickDecoder encodeTrick)
    -- this does not fulfill the property due to the special "bonzo" case:
    -- it "GameCommand" $
    --   property (jsonRoundtrip gameCommandDecoder encodeGameCommand)
    it "GameEvent" $
      property (jsonRoundtrip gameEventDecoder encodeGameEvent)
  describe "suitDecoder" $ do
    it "fails gracefully" $ do
      let Left (Decode.Failure message _) = decode suitDecoder "\"no suit\""
      message `shouldBe` "Not a suit: no suit"
  describe "rankDecoder" $ do
    it "fails gracefully" $ do
      let Left (Decode.Failure message _) = decode rankDecoder "\"no rank\""
      message `shouldBe` "Not a rank: no rank"

mapDecoderSpec :: Spec
mapDecoderSpec =
  describe "mapDecoder" $ do
    it "correctly decodes a single sample value" $ do
      let s =
            "[[\"bonzo\",\
            \  [{\"suit\" : \"Diamonds\", \"rank\" : \"Eight\"},\
            \   {\"suit\" : \"Clubs\", \"rank\" : \"Eight\"}]]]"
          d = mapDecoder playerDecoder (Decode.list cardDecoder)
          expected =
            Map.fromList
              [ ( Player "bonzo",
                  [Card Diamonds Eight, Card Clubs Eight]
                )
              ]
      decode d s `shouldBe` Right expected

dataDecoderSpec :: Spec
dataDecoderSpec =
  describe "dataDecoder" $ do
    it "correctly decodes a single sample value" $ do
      let d =
            dataDecoder
              ( Map.fromList
                  [ ("i", show <$> Decode.int),
                    ("s", Decode.string)
                  ]
              )
              (\tag -> Decode.faild ("Unknown tag: " ++ tag))
      decode d "{\"tag\" : \"s\", \"contents\" : \"foo\"}" `shouldBe` Right "foo"

dealHandsDecoderSpec :: Spec
dealHandsDecoderSpec =
  describe "dealHandsDecoder" $ do
    it "correctly decodes a single sample value" $ do
      let s =
            "[[\"bonzo\",\
            \  [{\"suit\" : \"Diamonds\", \"rank\" : \"Eight\"},\
            \   {\"suit\" : \"Clubs\", \"rank\" : \"Eight\"}]]]"
          expected =
            DealHands
              ( Map.fromList
                  [ ( Player "bonzo",
                      makeHand [Card Diamonds Eight, Card Clubs Eight]
                    )
                  ]
              )
      decode dealHandsDecoder s `shouldBe` Right expected

gameCommandDecoderSpec :: Spec
gameCommandDecoderSpec =
  describe "gameCommandDecoderSpec" $ do
    it "correctly identifies and decodes a single DealHands value" $ do
      let s =
            "{\"tag\" : \"DealHands\",\
            \ \"contents\" : [[\"bonzo\",\
            \                  [{\"suit\" : \"Diamonds\", \"rank\" : \"Eight\"},\
            \                   {\"suit\" : \"Clubs\", \"rank\" : \"Eight\"}]]]}"
          expected =
            DealHands
              ( Map.fromList
                  [ ( Player "bonzo",
                      makeHand [Card Diamonds Eight, Card Clubs Eight]
                    )
                  ]
              )
      decode gameCommandDecoder s `shouldBe` Right expected
