module QuantizeScale exposing (domain, derivedDomain, range, quantizeScale, lookupRange, lookupDomain, setLookup, transform)

import Category exposing (Category(..))
import Scale exposing (min, max)
import List exposing (..)


-- update


transform func model =
    func model


domain : List Float -> Model a -> Model a
domain list model =
    { model | domain = [ (Scale.min list), (Scale.max list) ] } |> derivedDomain list


derivedDomain : List Float -> Model a -> Model a
derivedDomain list model =
    { model | derivedDomain = [ (Scale.min list), (Scale.max list) ] }


range : List a -> Model a -> Model a
range list model =
    { model | range = list } |> setLookup


setLookup model =
    let
        newInterval =
            (Scale.max model.derivedDomain - Scale.min model.derivedDomain) / (toFloat (List.length model.range))

        newRange =
            List.indexedMap (,) model.range
    in
        { model | rangeInterval = newInterval, derivedRange = newRange }


lookupRange dp model =
    let
        k =
            floor (dp / model.rangeInterval)

        v =
            filter
                (\( x, y ) ->
                    if ( x, y ) == ( k, y ) then
                        True
                    else
                        False
                )
                model.derivedRange
                |> head
    in
        case v of
            Just ( x, y ) ->
                Just y

            Nothing ->
                Nothing


lookupDomain rp model =
    let
        v =
            filter
                (\( x, y ) ->
                    if ( x, y ) == ( x, rp ) then
                        True
                    else
                        False
                )
                model.derivedRange
                |> head
    in
        case v of
            Just ( x, y ) ->
                [ toFloat (x) * model.rangeInterval, toFloat (x + 1) * model.rangeInterval ]

            Nothing ->
                []



-- Model


type alias Model a =
    { category : Category
    , domain : List Float
    , derivedDomain : List Float
    , range : List a
    , derivedRange : List ( Int, a )
    , rangeInterval : Float
    }


category : Category
category =
    Quantize


defaultDomain : List Float
defaultDomain =
    []


defaultRange : List a
defaultRange =
    []


defaultDerivedRange : List ( Int, a )
defaultDerivedRange =
    []


quantizeScale : Model a
quantizeScale =
    { category = category
    , domain = defaultDomain
    , derivedDomain = defaultDomain
    , range = defaultRange
    , derivedRange = []
    , rangeInterval = 0
    }
