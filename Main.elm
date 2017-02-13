module Main exposing (..)

import Html exposing (..)
import OrdinalScale exposing (..)
import LinearScale exposing (..)
import PowerScale exposing (..)


--import Svg exposing (..)


test1 =
    powerScale 2 |> PowerScale.domain [ 1, 2 ] |> PowerScale.range [ 1, 10 ]


test2 =
    linearScale |> LinearScale.domain [ 10, 20 ] |> LinearScale.range [ 1, 10 ]


test3 =
    ordinalScale |> OrdinalScale.domain [ "aa", "bb", "cc" ] |> OrdinalScale.setLookup


main =
    div []
        [ div [] [ text (toString (test1)) ]
        , hr [] []
        , div [] [ text (toString (test2)) ]
        , hr [] []
        , div [] [ text (toString (test3)) ]
        ]
