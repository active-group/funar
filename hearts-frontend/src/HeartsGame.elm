module HeartsGame exposing (TableState, GameCommand(..), GameEvent(..), Player, PlayerName,
                            Card, Hand, Trick, Rank(..), Suit(..), PlayerHands,
                            emptyHand, emptyTableState, deck, cardColor, prettyCard,
                            tableProcessEvent)

import Dict exposing (Dict)

type Suit = Diamonds | Clubs | Spades | Hearts

-- |list of all suits
allSuits : List Suit
allSuits = [Spades, Hearts, Diamonds, Clubs]

type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

-- |list of all ranks
allRanks : List Rank
allRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
            Jack, Queen, King, Ace]

-- |playing cards
type alias Card = { suit : Suit,
                    rank : Rank }

-- |full deck of all cards
deck : List Card
deck = List.concat (List.map (\ rank -> List.map (\ suit -> Card suit rank) allSuits) allRanks)

-- |during the game, a hand contains at least one card
type alias Hand = List Card

-- |remove a card from a hand
removeCard : Card -> Hand -> Hand
removeCard card hand = List.filter (\ card0 -> card /= card0) hand

-- |empty hand
emptyHand : Hand
emptyHand = []

-- |pretty-print card rank
prettyRank : Rank -> String
prettyRank rank =
  case rank of
    Two -> "Two"
    Three -> "Three"
    Four -> "Four"
    Five -> "Five"
    Six -> "Six"
    Seven -> "Seven"
    Eight -> "Eight"
    Nine -> "Nine"
    Ten -> "Ten"
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"
    Ace -> "Ace"

-- |pretty-print card suit
prettySuit : Suit -> String
prettySuit suit =
  case suit of
    Diamonds -> String.fromChar '\u{1F0A1}'
    Clubs -> String.fromChar '\u{2665}'
    Spades -> String.fromChar '\u{2665}'
    Hearts -> String.fromChar '\u{2665}'

cardColor : Card -> String
cardColor c =
    case c.suit of
        Spades -> "black"
        Clubs -> "black"
        Hearts -> "red"
        Diamonds -> "red"

-- |pretty-print card
prettyCard : Card -> String
prettyCard c =
  String.fromChar
    (case (c.suit, c.rank) of
          (Spades, Two) -> '\u{1F0A2}'
          (Spades, Three) -> '\u{1F0A3}'
          (Spades, Four) -> '\u{1F0A4}'
          (Spades, Five) -> '\u{1F0A5}'
          (Spades, Six) -> '\u{1F0A6}'
          (Spades, Seven) -> '\u{1F0A7}'
          (Spades, Eight) -> '\u{1F0A8}'
          (Spades, Nine) -> '\u{1F0A9}'
          (Spades, Ten) -> '\u{1F0AA}'
          (Spades, Jack) -> '\u{1F0AB}'
          (Spades, Queen) -> '\u{1F0AD}'
          (Spades, King) -> '\u{1F0AE}'
          (Spades, Ace) -> '\u{1F0A1}'

          (Hearts, Two) -> '\u{1F0B2}'
          (Hearts, Three) -> '\u{1F0B3}'
          (Hearts, Four) -> '\u{1F0B4}'
          (Hearts, Five) -> '\u{1F0B5}'
          (Hearts, Six) -> '\u{1F0B6}'
          (Hearts, Seven) -> '\u{1F0B7}'
          (Hearts, Eight) -> '\u{1F0B8}'
          (Hearts, Nine) -> '\u{1F0B9}'
          (Hearts, Ten) -> '\u{1F0BA}'
          (Hearts, Jack) -> '\u{1F0BB}'
          (Hearts, Queen) -> '\u{1F0BD}'
          (Hearts, King) -> '\u{1F0BE}'
          (Hearts, Ace) -> '\u{1F0B1}'

          (Diamonds, Two) -> '\u{1F0C2}'
          (Diamonds, Three) -> '\u{1F0C3}'
          (Diamonds, Four) -> '\u{1F0C4}'
          (Diamonds, Five) -> '\u{1F0C5}'
          (Diamonds, Six) -> '\u{1F0C6}'
          (Diamonds, Seven) -> '\u{1F0C7}'
          (Diamonds, Eight) -> '\u{1F0C8}'
          (Diamonds, Nine) -> '\u{1F0C9}'
          (Diamonds, Ten) -> '\u{1F0CA}'
          (Diamonds, Jack) -> '\u{1F0CB}'
          (Diamonds, Queen) -> '\u{1F0CD}'
          (Diamonds, King) -> '\u{1F0CE}'
          (Diamonds, Ace) -> '\u{1F0C1}'

          (Clubs, Two) -> '\u{1F0D2}'
          (Clubs, Three) -> '\u{1F0D3}'
          (Clubs, Four) -> '\u{1F0D4}'
          (Clubs, Five) -> '\u{1F0D5}'
          (Clubs, Six) -> '\u{1F0D6}'
          (Clubs, Seven) -> '\u{1F0D7}'
          (Clubs, Eight) -> '\u{1F0D8}'
          (Clubs, Nine) -> '\u{1F0D9}'
          (Clubs, Ten) -> '\u{1F0DA}'
          (Clubs, Jack) -> '\u{1F0DB}'
          (Clubs, Queen) -> '\u{1F0DD}'
          (Clubs, King) -> '\u{1F0DE}'
          (Clubs, Ace) -> '\u{1F0D1}')

-- |pretty-print list of cards
prettyCards : List Card -> String
prettyCards cards =
  case cards of
    [] -> ""
    [x] -> prettyCard x
    (x::xs) -> prettyCard x ++ " and\n" ++ prettyCards xs

type alias PlayerName = String
type alias Player = { name : PlayerName }

type alias PlayerPiles = Dict PlayerName (List Card)
type alias PlayerHands  = Dict PlayerName Hand

type alias Trick = List (Player, Card)

emptyTrick : Trick
emptyTrick = []

type alias TableState =
  { players : List Player
  , hands : PlayerHands
  , piles : PlayerPiles
  , trick : Trick
  , winner : Maybe Player
  }

emptyPlayerHands : PlayerHands
emptyPlayerHands = Dict.empty

emptyPlayerPiles : List Player -> PlayerPiles
emptyPlayerPiles players =
    Dict.fromList (List.map (\ player -> (player.name, [])) players)

emptyTableState : List Player -> TableState
emptyTableState players =
  { players = players
  , hands = emptyPlayerHands
  , piles = emptyPlayerPiles players
  , trick = emptyTrick
  , winner = Nothing
  }

type GameEvent =
    HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player

type GameCommand =
    DealHands PlayerHands
  | PlayCard Player Card

tableProcessEvent : GameEvent -> TableState -> TableState
-- tableProcessEvent event state | trace ("tableProcessEvent " ++ show state ++ " " ++ show event) False = undefined
tableProcessEvent event state =
  case event of
    HandDealt player hand ->
      { state | hands = Dict.insert player.name hand state.hands,
                trick = emptyTrick }
-- exercise: leave out cases
    PlayerTurnChanged player ->
      { state | players = rotateTo player state.players }
    LegalCardPlayed player card ->
      { state | players = rotate (rotateTo player state.players),
                hands = takeCard state.hands player card,
                piles = state.piles,
                trick = addToTrick player card state.trick }
    TrickTaken player trick ->
      { state | piles = addToPile state.piles player (cardsOfTrick trick),
                trick = emptyTrick }
    IllegalCardAttempted _ _ -> state
    GameEnded player ->
      { state | winner = Just player }

-- |rotate assumes length of input > 0
rotate : List a -> List a
rotate l =
  case l of
    (x :: xs) -> xs ++ [x]
    [] -> Debug.todo "bug"

-- |rotateTo assumes target exists in input of length > 0
rotateTo : a -> List a -> List a
rotateTo y xs =
  case xs of
    x :: xsR -> if x == y
                then xs
                else rotateTo y (xsR ++ [x])
    [] -> Debug.todo "bug"

takeCard : PlayerHands -> Player -> Card -> PlayerHands
takeCard playerHand player card =
  Dict.update player.name (Maybe.map (removeCard card)) playerHand

addToTrick : Player -> Card -> Trick -> Trick
addToTrick player card trick = (player, card) :: trick

addToPile : PlayerPiles -> Player -> List Card -> PlayerPiles
addToPile playerPiles player cards =
  let updatePile m = Maybe.map (\ pile -> pile ++ cards) m
  in Dict.update player.name updatePile playerPiles

cardsOfTrick : Trick -> List Card
cardsOfTrick trick = List.map (\ (_, card) -> card) trick
