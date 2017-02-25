module PathFinder exposing (findPaths)

import Types exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)


findPaths : Int -> BoardDict -> String -> List Path
findPaths boardWidth board guess =
    List.filter (\path -> List.length path == String.length guess) <|
        List.concat <|
            (List.map
                (\point -> (explorePath boardWidth board [ point ] (shortenedWord guess)))
             <|
                Dict.keys <|
                    firstLetterMatches guess board
            )


firstLetterMatches : String -> BoardDict -> BoardDict
firstLetterMatches guess board =
    let
        matchFirstLetter : Point -> Tile -> Tile
        matchFirstLetter point tile =
            checkTile (firstLetter guess) tile
    in
        Dict.filter isMatching <|
            Dict.map matchFirstLetter board


firstLetter : String -> String
firstLetter string =
    String.slice 0 1 string


shortenedWord : String -> String
shortenedWord word =
    String.dropLeft 1 word


checkTile : String -> Tile -> Tile
checkTile guessLetter tile =
    { tile | match = (guessLetter == tile.letter) }


isMatching : Point -> Tile -> Bool
isMatching _ tile =
    tile.match


explorePath : Int -> BoardDict -> Path -> String -> List Path
explorePath boardWidth board path word =
    let
        lastPoint : Path -> Maybe Point
        lastPoint path =
            Array.get 0 (Array.fromList path)

        travel : BoardDict -> String -> Path -> List Path
        travel board word path =
            List.map
                (\match ->
                    if (not <| List.member match path) then
                        List.append [ match ] path
                    else
                        []
                )
                (matchingNeighbors boardWidth board (lastPoint path) (firstLetter word))
    in
        if String.length word > 0 then
            List.concatMap (\aPath -> (explorePath boardWidth board aPath <| shortenedWord word)) (travel board word path)
        else
            [ path ]


matchingNeighbors : Int -> BoardDict -> Maybe Point -> String -> List Point
matchingNeighbors boardWidth board point letter =
    let
        isMatch : Point -> Bool
        isMatch point =
            (Maybe.withDefault { letter = "", match = False } <| Dict.get point board).letter == letter
    in
        case point of
            Just value ->
                List.filter isMatch (getNeighbors boardWidth value)

            Nothing ->
                []


getNeighbors : Int -> Point -> List Point
getNeighbors boardWidth ( x, y ) =
    Set.toList <|
        Set.remove ( x, y ) <|
            Set.fromList
                [ ( max (x - 1) 0, max (y - 1) 0 )
                , ( max (x - 1) 0, y )
                , ( max (x - 1) 0, min (y + 1) boardWidth )
                , ( x, max (y - 1) 0 )
                , ( x, min (y + 1) boardWidth )
                , ( min (x + 1) boardWidth, max (y - 1) 0 )
                , ( min (x + 1) boardWidth, y )
                , ( min (x + 1) boardWidth, min (y + 1) boardWidth )
                ]
