module Main exposing (..)

import Html exposing (..)
import Scale exposing (..)


--import Svg exposing (..)


test =
    linearScale |> domain [ 4, 9 ] |> range [ 1, 10 ]


main =
    text (toString (test.category) ++ "\x0D\n" ++ toString (test.linear))
