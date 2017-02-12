module Main exposing (..)

import Html exposing (..)


--import OrdinalScale exposing (..)

import LinearScale exposing (..)


--import Svg exposing (..)


test =
    linearScale |> domain [ 1, 3, 4, 5, 7 ] |> setLookup


main =
    text (toString (test))
