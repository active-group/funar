module TableSpec (spec) where

import Cards
import GameEvent
import Table
import Shuffle

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Data.Set as Set

import Test.Hspec

spec :: Spec
spec = do
  runTableSpec

mike = Player "Mike"
nicole = Player "Nicole"
peter = Player "Peter"
annette = Player "Annette"
players = [mike, nicole, peter, annette]

playerHands = Map.fromList (zip players (map makeHand (distribute 4 deck)))

dealCommand = DealHands playerHands

state0 = emptyTableState players

game = tableLoopM dealCommand

eventsToPlayerHands :: [GameEvent] -> Map Player Hand
eventsToPlayerHands events =
  let f hands (HandDealt player hand) = Map.insert player hand hands
      f hands _ = hands
  in foldl f Map.empty events

runTableSpec :: Spec
runTableSpec =
  describe "DealHands" $ do
    it "correctly generate HandDealt events from DealHands command" $
      do let (result, state1, events1) = runTable game (state0, [])
         eventsToPlayerHands events1 `shouldBe` playerHands
