module Main exposing (..)

import Task exposing (Task)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput, on)
import Dict exposing (Dict)
import Dom exposing (focus, Error)
import Http
import Json.Decode as Decode
import Keyboard exposing (presses)
import Char exposing (fromCode)
import Time
import BoardRandomizer
import PathFinder exposing (findPaths)
import Types exposing (..)


-- APP


main : Program Flags Model Msg
main =
    Html.programWithFlags { init = init, subscriptions = subscriptions, view = view, update = update }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses
            (\code -> Presses (Char.fromCode code))
        , Time.every Time.second Tick
        ]



-- HTTP


authenticatedGet : String -> Decode.Decoder String -> Http.Request String
authenticatedGet url decoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Mashape-Key" "7RhzLqB7x0mshfh5YE2afEP7Ngkxp1xigQqjsnKy6oDuQ7CfkC" ]
        , body = Http.emptyBody
        , url = url
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


lookUpWord : String -> Cmd Msg
lookUpWord word =
    let
        url =
            "https://wordsapiv1.p.mashape.com/words/" ++ word
    in
        Http.send DefineWord (authenticatedGet url decodeResponse)


decodeResponse : Decode.Decoder String
decodeResponse =
    Decode.at [ "word" ] Decode.string



-- MODEL


type alias Model =
    { board : BoardDict
    , score : Int
    , currentGuess : String
    , foundWords : List String
    , hasMatch : Bool
    , guessed : Bool
    , correct : Maybe Bool
    , definition : Maybe String
    , ticks : Int
    }


type alias Flags =
    { startTime : Int
    }


model : Model
model =
    { board = getBoardDict <| [ [ { letter = "", match = False } ] ]
    , score = 0
    , currentGuess = ""
    , foundWords = []
    , hasMatch = False
    , guessed = False
    , correct = Nothing
    , definition = Nothing
    , ticks = 0
    }


boardWidth : Int
boardWidth =
    5


createBoard : Int -> BoardDict
createBoard seed =
    let
        letters =
            BoardRandomizer.createRandomBoard boardWidth seed

        tilesForRow : List String -> Row
        tilesForRow row =
            List.map tileForLetter row

        tileForLetter : String -> Tile
        tileForLetter letter =
            { letter = letter, match = False }
    in
        getBoardDict <| List.map tilesForRow letters



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { model
        | board = createBoard flags.startTime
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ScoreWord
    | UpdateGuessWord String
    | Presses Char
    | DefineWord (Result Http.Error String)
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Presses code ->
            if code == '\x0D' then
                (update ScoreWord model)
            else
                (update NoOp model)

        Tick time ->
            ( { model
                | ticks = model.ticks + 1
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        ScoreWord ->
            let
                isValidGuess =
                    model.hasMatch && not (List.member model.currentGuess model.foundWords)

                isCorrect =
                    if isValidGuess then
                        Just True
                    else
                        Nothing
            in
                ( { model
                    | guessed = True
                    , currentGuess =
                        if isValidGuess then
                            model.currentGuess
                        else
                            ""
                    , correct = isCorrect
                  }
                , if isValidGuess then
                    lookUpWord model.currentGuess
                  else
                    Cmd.none
                )

        DefineWord (Ok definition) ->
            ( { model
                | definition = Just (definition)
                , currentGuess = ""
                , correct = Just True
                , score = model.score + (String.length model.currentGuess)
                , foundWords =
                    if not (List.member model.currentGuess model.foundWords) then
                        model.currentGuess :: model.foundWords
                    else
                        model.foundWords
              }
            , Task.attempt (always NoOp) (Dom.focus "guess-input")
            )

        DefineWord (Err _) ->
            ( { model
                | definition = Nothing
                , currentGuess = ""
                , correct = Just False
              }
            , Task.attempt (always NoOp) (Dom.focus "guess-input")
            )

        UpdateGuessWord guess ->
            let
                findMatches : Point -> Tile -> Tile
                findMatches point tile =
                    if List.member point (Maybe.withDefault [] (List.head <| findPaths boardWidth model.board guess)) then
                        { tile | match = True }
                    else
                        tile

                newDict =
                    Dict.map findMatches <| clearBoard model.board
            in
                ( { model
                    | currentGuess =
                        guess
                    , board = newDict
                    , hasMatch = not <| Dict.isEmpty (Dict.filter (\key tile -> tile.match == True) newDict)
                    , guessed = False
                  }
                , Cmd.none
                )


clearBoard : BoardDict -> BoardDict
clearBoard board =
    let
        clearTile : Point -> Tile -> Tile
        clearTile point tile =
            { tile
                | match = False
            }
    in
        Dict.map clearTile board


getBoardDict : Board -> BoardDict
getBoardDict board =
    let
        encodeTile : Int -> Int -> Tile -> ( Point, Tile )
        encodeTile x y tile =
            ( ( x, y ), tile )

        encodeRow : Int -> Row -> List ( Point, Tile )
        encodeRow index row =
            List.indexedMap (encodeTile index) row
    in
        Dict.fromList <| List.concatMap (\n -> n) (List.indexedMap encodeRow board)



-- VIEW


view : Model -> Html Msg
view model =
    let
        makeRow row =
            div [] (List.map makeTile row)

        makeTile tile =
            span [ classList [ ( "letter", True ), ( "letter--highlighted", tile.match ) ] ] [ text tile.letter ]

        makeFoundWord word =
            div [ class "foundWord" ] [ text word ]
    in
        div [ class "gameRoot" ]
            [ div [ class "header" ]
                [ h2 [ class "timer" ] [ text <| "" ++ toString (model.ticks) ]
                , h1 [ class "title" ]
                    [ a [ Html.Attributes.href "http://elm-lang.org/" ] [ text "Elm" ]
                    , text " Boggle"
                    ]
                ]
            , div [ classList [ ( "game", True ), ( "guessed", model.guessed ), ( "pending", model.correct == Nothing ), ( "correct", model.correct == Just True ) ] ]
                [ div []
                    [ h2 [] [ text <| "Score: " ++ toString model.score ]
                    , div [ class "boardContainer" ] (List.map makeTile <| Dict.values model.board)
                    , div [ class "controls" ]
                        [ input
                            [ class "guesser"
                            , placeholder "Guess away!"
                            , onInput UpdateGuessWord
                            , value model.currentGuess
                            , Html.Attributes.autofocus True
                            ]
                            []
                        , button [ class "checker", onClick ScoreWord ] [ text "Check" ]
                        ]
                    ]
                , div [ class "foundWordContainer" ] <| [ h2 [] [ text "Found Words" ] ] ++ (List.map makeFoundWord model.foundWords)
                ]
            , div [ class "footer" ]
                [ a [ Html.Attributes.href "https://github.com/jeanettehead/lady-boggle" ] [ text "See the code on GitHub" ]
                , a [ Html.Attributes.href "http://iamjea.net" ] [ text "My Website" ]
                ]
            ]
