module Types exposing (..)

import Dict exposing (Dict)


type alias Board =
    List Row


type alias Tile =
    { letter : String, match : Bool }


type alias Point =
    ( Int, Int )


type alias Row =
    List Tile


type alias StartingPoints =
    List Point


type alias Path =
    List Point


type alias Paths =
    List Path


type alias BoardDict =
    Dict Point Tile
