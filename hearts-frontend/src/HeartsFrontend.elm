module HeartsFrontend exposing (main)

import Browser
import Html exposing (Html, span, div, button, ul, li, br, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http

import Json.Decode exposing (Decoder)

import Dict
import Random
import Task exposing (Task)

import HeartsGame exposing (..)
import HeartsJson exposing (..)
import Shuffle

type alias Endpoint = String

tableServerEndpointUri : Endpoint
tableServerEndpointUri =
    "http://localhost:8080/command"

playersWithEndpoints : List (Player, Endpoint)
playersWithEndpoints =
    [ (Player "Mike", "http://localhost:8001/event")
    , (Player "Peter", "http://localhost:8002/event")
    , (Player "Nicole", "http://localhost:8003/event")
    , (Player "Annette", "http://localhost:8004/event")
    ]

playerEndpointUris : List Endpoint
playerEndpointUris =
    List.map
        Tuple.second
        playersWithEndpoints

allPlayers : List Player
allPlayers =
    List.map
        Tuple.first
        playersWithEndpoints

randomSeedGenerator : Random.Generator Random.Seed
randomSeedGenerator =
    Random.map
        Random.initialSeed
        (Random.int 0 123456)

findPlayer : PlayerName -> List Player -> Player
findPlayer playerName players =
    let candidates = List.filter (\ player -> player.name == playerName) players
    in
        case List.head candidates of
            Just player -> player
            Nothing -> Debug.todo ("Fatal: no player with ID " ++ playerName ++ " found!")

-- MAIN

main =
  Browser.element
      { init = init
      , update = update
      , view = view
      , subscriptions = subscriptions
      }

-- MODEL

type alias Model =
  { tableState : TableState
  , gameCommands : List GameCommand
  }

init : () -> (Model, Cmd Msg)
init =
    \_ -> ({ tableState = emptyTableState allPlayers
           , gameCommands = []
           }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

type Msg = Deal Hand Hand Hand Hand
         | ProcessGameCommands
         | GotGameEvents (Result Http.Error (List GameEvent))
         | GotGameCommands (Result Http.Error (List GameCommand))
         | ShuffleCards

postGameCommand : GameCommand -> Cmd Msg
postGameCommand gameCommand =
    Http.post { url = tableServerEndpointUri
              , body = Http.jsonBody (encodeGameCommand gameCommand)
              , expect = Http.expectJson GotGameEvents gameEventsDecoder
              }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      ShuffleCards ->
          (model, Random.generate seedToDealHands randomSeedGenerator)
      Deal hand1 hand2 hand3 hand4 ->
          ( model
          , postGameCommand (DealHands (Dict.fromList
                                        [("Mike", hand1)
                                        ,("Peter", hand2)
                                        ,("Nicole", hand3)
                                        ,("Annette", hand4)])))
      GotGameEvents (Ok gameEvents) ->
          ( {model | tableState = List.foldl tableProcessEvent model.tableState gameEvents}
          , Task.attempt GotGameCommands (forwardGameEventsSequentially playerEndpointUris gameEvents))
      GotGameEvents (Err err) ->
          Debug.todo (showHttpError err)
      GotGameCommands (Ok commands) ->
          ( {model | gameCommands = model.gameCommands ++ commands}, Cmd.none )
      GotGameCommands (Err err) ->
          Debug.todo (showHttpError err)
      ProcessGameCommands ->
          ( {model | gameCommands = []}
          , Cmd.batch (List.map postGameCommand model.gameCommands))

seedToDealHands : Random.Seed -> Msg
seedToDealHands seed =
    case distribute (Shuffle.shuffle seed deck) of
        [hand1, hand2, hand3, hand4] -> Deal hand1 hand2 hand3 hand4
        _ -> Debug.todo "bug in the program"

extract4 : List a -> List a
extract4 ys =
    case ys of
        [] -> []
        z :: zs -> z :: extract4 (List.drop 3 zs)

distribute : List a -> List (List a)
distribute xs =
    List.map (\i -> extract4 (List.drop i xs)) [0, 1, 2, 3]

handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)
        Http.Timeout_ ->
            Err Http.Timeout
        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)
        Http.NetworkError_ ->
            Err Http.NetworkError
        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)
                Ok result ->
                    Ok result

forwardGameEventsSequentially : List Endpoint -> List GameEvent -> Task Http.Error (List GameCommand)
forwardGameEventsSequentially endpoints gameEvents =
    let pairs = combine endpoints gameEvents
    in
        Task.map List.concat
            (Task.sequence
            (List.map
                 (\ (endpoint, gameEvent) -> gameEventToTask endpoint gameEvent)
                 pairs))

combine : List a -> List b -> List (a, b)
combine xs ys =
    List.concatMap (\ x -> List.map (Tuple.pair x) ys) xs

gameEventToTask : Endpoint -> GameEvent -> Task Http.Error (List GameCommand)
gameEventToTask endpoint gameEvent =
    Http.task { method = "POST"
              , headers = []
              , url = endpoint
              , body = Http.jsonBody (encodeGameEvent gameEvent)
              , resolver = Http.stringResolver <| handleJsonResponse <| gameCommandsDecoder
              , timeout = Nothing
              }

showHttpError : Http.Error -> String
showHttpError err =
  case err of
    Http.BadUrl s -> "bad url: " ++ s
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus status -> "bad status: " ++ String.fromInt status
    Http.BadBody s -> "bad body: " ++ s

-- VIEW

prettyColoredCard : Card -> Html Msg
prettyColoredCard card =
    span [ style "color" (cardColor card) ]
         [ text (prettyCard card) ]

floatingPrettyColoredCard : Card -> Html Msg
floatingPrettyColoredCard card =
    span [ style "float" "left"
         , style "margin" "5px"
         ]
         [ prettyColoredCard card ]

showHand : Hand -> Html Msg
showHand hand =
    div [] (List.map floatingPrettyColoredCard hand)

showPlayer : Player -> Hand -> Html Msg
showPlayer player hand =
    div [ style "float" "left"
         , style "height" "100px"
         , style "width" "200px"
         ]
         [ text player.name
         , showHand hand
        ]

prettyGameCommand : GameCommand -> Html Msg
prettyGameCommand gameCommand =
    case gameCommand of
        DealHands _ ->
            Debug.todo "should be unnecessary"
        PlayCard player card ->
            let t = "Player " ++ player.name ++ " wants to play "
            in
                div []
                    [ span [ style "float" "left"
                           , style "margin-right" "5px"
                           ]
                           [ text t ]
                    , prettyColoredCard card
                    ]

showGameCommands : List GameCommand -> Html Msg
showGameCommands gameCommands =
     div [ style "height" "100px" ]
         [ text "Pending commands:"
         , ul [] (List.map (\ gameCommand -> li [] [ prettyGameCommand gameCommand ]) gameCommands)
         ]

showTrick : Trick -> Html Msg
showTrick trick =
     div [ style "margin-top" "20px"
         , style "height" "100px"
         ]
         [ text "Current trick:"
         , div [] (List.reverse (List.map
                                    (floatingPrettyColoredCard << Tuple.second)
                                    trick))
         ]

showWinner : Maybe Player -> List Player -> Html Msg
showWinner maybeWinner players =
    case maybeWinner of
        Just winner ->
            let realWinner = findPlayer winner.name players
            in
                div [ style "color" "green"
                    , style "font-weight" "bold"
                    ]
                    [ text (realWinner.name ++ " has won the game!") ]
        Nothing ->
            div [] []

view : Model -> Html Msg
view { tableState, gameCommands } =
    let playersWithHands = List.map (\ player -> case Dict.get player.name tableState.hands of
                                                     Just hand -> (player, hand)
                                                     Nothing -> (player, emptyHand)) tableState.players
    in
    div [style "font-family" "sans-serif"]
        (List.concat [ [ div [ style "width" "100%"
                             , style "background-color" "#ccc"
                             ]
                             [ button [ onClick ShuffleCards
                                      , style "margin" "10px"
                                      ]
                                   [ text "Deal cards" ]
                             , button [ onClick ProcessGameCommands
                                      , style "margin" "5px"
                                      ]
                                   [ text "Process commands" ]] ]
                     , [ br [ style "clear" "both" ] [] ]
                     , List.map (\ (player, hand) -> showPlayer player hand) playersWithHands
                     , [ br [ style "clear" "both" ] [] ]
                     , [ showGameCommands gameCommands ]
                     , [ br [ style "clear" "both" ] [] ]
                     , [ showTrick tableState.trick ]
                     , [ br [ style "clear" "both" ] [] ]
                     , [ showWinner tableState.winner allPlayers ]
                     ])
