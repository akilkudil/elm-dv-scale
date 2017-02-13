module Main exposing (..)

import Html exposing (..)
import OrdinalScale exposing (..)
import LinearScale exposing (..)
import PowerScale exposing (..)
import SqrtScale exposing (..)


--import Svg exposing (..)


func model =
    model


linearTransform model =
    { model | domain = (List.map (\x -> x + 100) model.domain) }


test1 =
    powerScale 2 |> PowerScale.domain [ 1, 2 ] |> PowerScale.range [ 1, 10 ]


test2 =
    linearScale |> LinearScale.domain [ 10, 20 ] |> LinearScale.range [ 1, 10 ] |> LinearScale.transform linearTransform


test3 =
    ordinalScale |> OrdinalScale.domain [ "aa", "bb", "cc" ] |> OrdinalScale.setLookup |> OrdinalScale.transform func


test4 =
    sqrtScale |> SqrtScale.domain [ 1, 2 ] |> SqrtScale.range [ 1, 10 ]


main =
    div []
        [ div [] [ text (toString (test1)) ]
        , hr [] []
        , div [] [ text (toString (test2)) ]
        , hr [] []
        , div [] [ text (toString (test3)) ]
        , hr [] []
        , div [] [ text (toString (test4)) ]
        ]
