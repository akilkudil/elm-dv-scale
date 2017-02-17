module Main exposing (..)

import Html exposing (..)
import OrdinalScale exposing (..)
import LinearScale exposing (..)
import PowerScale exposing (..)
import SqrtScale exposing (..)
import LogScale exposing (..)
import QuantizeScale exposing (..)
import QuantileScale exposing (..)


--import Svg exposing (..)


main =
    div []
        [ div [] [ text (toString (test1)) ]
        , hr [] []
        , div [] [ text (toString (test2)) ]
        , hr [] []
        , div [] [ text (toString (test3)) ]
        , hr [] []
        , div [] [ text (toString (test4)) ]
        , hr [] []
        , div [] [ text (toString (test5)) ]
        , hr [] []
        , div [] [ text (toString (test6)) ]
        , hr [] []
        , div [] [ text (toString (test6 |> QuantizeScale.lookupRange  1  )) ]
        , hr [] []
        , div [] [ text (toString (test7)) ]
        , hr [] []
        , div [] [ text (toString (test7 |> QuantileScale.lookupDomain "blue" )) ]
        ]


func model =
    model


linearTransform model =
    { model | domain = (List.map (\x -> x + 100) model.domain) }


test1 =
    powerScale 2 |> PowerScale.domain [ 1, 2 ] |> PowerScale.range [ 1, 10 ]


test2 =
    linearScale |> LinearScale.domain [ 10, 20 ] |> LinearScale.range [ 1, 10 ] |> LinearScale.transform linearTransform


test3 =
    ordinalScale |> OrdinalScale.domain [ "aa", "bb", "cc" ] |> OrdinalScale.range [4,6,8] |> OrdinalScale.setLookup |> OrdinalScale.lookupDomain 6


test4 =
    sqrtScale |> SqrtScale.domain [ 1, 2 ] |> SqrtScale.range [ 1, 10 ]


test5 =
    logScale 10 |> LogScale.domain [ 1, 10 ] |> LogScale.range [ 0, 1 ]


test6 =
    quantizeScale |> QuantizeScale.domain [ 0, 10 ] |> QuantizeScale.range [ "small", "medium", "large" ]


test7 =
    quantileScale |> QuantileScale.domain [ 1, 3, 4, 5, 2, 7, 8, 9 ] |> QuantileScale.range [ "red", "green", "blue", "yellow", "violet" ] |> QuantileScale.setLookup
