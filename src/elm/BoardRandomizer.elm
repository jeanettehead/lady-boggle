port module BoardRandomizer exposing (createRandomBoard)

import Html
import Random exposing (int, initialSeed, generate, step)


createRandomBoard : Int -> Int -> List (List String)
createRandomBoard width seed =
    List.map
        (\row -> List.map decodeNumberToLetter row)
        ((groupInto width
            (randomSequence width seed)
         )
        )


randomSequence : Int -> Int -> List Int
randomSequence width seed =
    let
        gen =
            Random.list (width * width) (Random.int 0 150)

        s =
            initialSeed seed

        ( res, ns ) =
            step gen s
    in
        res


groupInto : Int -> List a -> List (List a)
groupInto groups initial =
    let
        len =
            List.length initial

        n =
            len // groups
    in
        List.repeat groups []
            |> List.indexedMap
                (\i _ ->
                    List.take n (List.drop (n * i) initial)
                )


decodeNumberToLetter : Int -> String
decodeNumberToLetter num =
    if num < 19 then
        "e"
    else if num < 32 then
        "t"
    else if num < 44 then
        "a"
    else if num < 56 then
        "r"
    else if num < 67 then
        "i"
    else if num < 78 then
        "n"
    else if num < 89 then
        "o"
    else if num < 98 then
        "s"
    else if num < 104 then
        "d"
    else if num < 109 then
        "c"
    else if num < 114 then
        "h"
    else if num < 119 then
        "l"
    else if num < 123 then
        "f"
    else if num < 127 then
        "m"
    else if num < 131 then
        "p"
    else if num < 135 then
        "u"
    else if num < 138 then
        "g"
    else if num < 141 then
        "y"
    else if num < 143 then
        "w"
    else if num < 144 then
        "b"
    else if num < 145 then
        "j"
    else if num < 146 then
        "k"
    else if num < 147 then
        "q"
    else if num < 148 then
        "v"
    else if num < 149 then
        "x"
    else
        "z"



--frequencies
-- 19 e
-- 13 t 32
-- 12 a 44
-- 12 r 56
-- 11 i 67
-- 11 n 78
-- 11 o 89
--  9 s 98
--  6 d 104
--  5 c 109
--  5 h 114
--  5 l 119
--  4 f 123
--  4 m 127
--  4 p 131
--  4 u 135
--  3 g 138
--  3 y 141
--  2 w 143
--  1 b 144
--  1 j 145
--  1 k 146
--  1 q 147
--  1 v 148
--  1 x 149
--  1 z 150
