{-# LANGUAGE OverloadedStrings #-}

module Json.DecodeSpec (spec) where

import qualified Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators (mapLeft)
import Json.Decode (DecodeError (..), Decoder (..))
import qualified Json.Decode as Decode
import Test.Hspec

decode :: Decoder a -> ByteString -> Either (Either String DecodeError) a
decode decoder bs =
  case Data.Aeson.eitherDecode bs of
    Left err -> Left (Left err)
    Right json -> mapLeft Right (runDecoder decoder json)

spec :: Spec
spec = do
  decoderSpecs
  applicativeSpec

decoderSpecs :: Spec
decoderSpecs = do
  describe "string" $ do
    it "can be decoded" $
      decode Decode.string "\"foo\"" `shouldBe` Right "foo"
  describe "int" $ do
    it "can be decoded" $
      decode Decode.int "157" `shouldBe` Right 157
  describe "array" $ do
    it "can be decoded" $
      decode (Decode.list Decode.int) "[1,2,5]" `shouldBe` Right [1, 2, 5]
  describe "oneOf" $ do
    it "can be decoded" $ do
      let oneOf = Decode.oneOf [fmap show Decode.int, Decode.string]
      decode oneOf "123" `shouldBe` Right "123"
      decode oneOf "\"123\"" `shouldBe` Right "123"
      decode oneOf "true"
        `shouldBe` Left
          ( Right
              ( OneOf
                  [ Failure "Not an int" (Data.Aeson.Bool True),
                    Failure "Not a string" (Data.Aeson.Bool True)
                  ]
              )
          )
  describe "optional" $ do
    it "can be decoded" $ do
      let optionalInt = Decode.optional Decode.int
      decode optionalInt "1234" `shouldBe` Right (Just 1234)
      decode optionalInt "null" `shouldBe` Right Nothing
  describe "index" $ do
    it "can be decoded" $ do
      let thirdElem = Decode.index 2 Decode.int
      decode thirdElem "[1,2,3,4]" `shouldBe` Right 3
  describe "field" $ do
    it "can be decoded" $ do
      let intFoo = Decode.field "foo" Decode.int
          boolBar = Decode.field "bar" Decode.bool
          decoder = do
            foo <- intFoo
            bar <- boolBar
            pure (foo, bar)
      decode decoder "{\"bar\":false,\"foo\":23}" `shouldBe` Right (23, False)

applicativeSpec :: Spec
applicativeSpec =
  describe "using the Applicative instance to map over multiple decoders" $
    it "works" $ do
      let decoder =
            (\a b -> (a + 1, b + 2))
              <$> Decode.index 0 Decode.int
              <*> Decode.index 1 Decode.int
      decode decoder "[1,2]" `shouldBe` Right (2, 4)
