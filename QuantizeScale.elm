module QuantizeScale exposing (domain, derivedDomain, range, quantizeScale, lookupRange, lookupDomain, setLookup, transform)

import Category exposing (Category(..))
import Scale exposing (min, max)
import List exposing (..)
import Dict exposing (..)


-- update


transform func model =
    func model


domain : List Float -> Model comparable -> Model comparable
domain list model =
    { model | domain = [ (Scale.min list), (Scale.max list) ] } |> derivedDomain list


derivedDomain : List Float -> Model comparable -> Model comparable
derivedDomain list model =
    { model | derivedDomain = [ (Scale.min list), (Scale.max list) ] }


range : List comparable -> Model comparable -> Model comparable
range list model =
    { model | range = list } |> setLookup


setLookup : Model comparable -> Model comparable
setLookup model =
    let
        newInterval =
            (Scale.max model.derivedDomain - Scale.min model.derivedDomain) / (toFloat (List.length model.range))

        newLookup =
            Dict.fromList (List.indexedMap (,) model.range)

        newReverseLookup =
            Dict.fromList (List.map2 (,) model.range (List.range 0 (List.length model.range)))
    in
        { model | rangeInterval = newInterval, lookup = newLookup, reverseLookup = newReverseLookup }


lookupRange dp model =
    let
        k =
            floor (dp / model.rangeInterval)

        v =
            Dict.get k model.lookup
    in
        v



--lookupDomain: comparable -> Int


lookupDomain rp model =
    let
        v =
            Dict.get rp model.reverseLookup
    in
        v



-- Model


type alias Model comparable =
    { category : Category
    , domain : List Float
    , derivedDomain : List Float
    , range : List comparable
    , lookup : Dict Int comparable
    , reverseLookup : Dict comparable Int
    , rangeInterval : Float
    }


category : Category
category =
    Quantize


defaultDomain : List Float
defaultDomain =
    []


defaultRange : List comparable
defaultRange =
    []


defaultLookup : Dict Int comparable
defaultLookup =
    Dict.empty


defaultReverseLookup : Dict a Int
defaultReverseLookup =
    Dict.empty


quantizeScale : Model comparable
quantizeScale =
    { category = category
    , domain = defaultDomain
    , derivedDomain = defaultDomain
    , range = defaultRange
    , lookup = defaultLookup
    , reverseLookup = defaultReverseLookup
    , rangeInterval = 0
    }
