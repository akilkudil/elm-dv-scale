module LinearScale exposing (domain, range, linearScale, lookupRange, setLookup)

import Category exposing (Category(..))
import List exposing (..)
import Dict exposing (..)


-- update


domain : List Float -> Model -> Model
domain list model =
    { model | domain = list }


range : List Float -> Model -> Model
range list model =
    { model | range = list } |> setLookup


lookupRange : Float -> Model -> Float
lookupRange dp model =
    let
        rangeValue =
            Dict.get dp model.lookup
    in
        case rangeValue of
            Nothing ->
                (model.a * dp + model.b)

            Just x ->
                x


setLookup model =
    let
        firstDomain =
            case (List.minimum model.domain) of
                Nothing ->
                    0

                Just x ->
                    x

        lastDomain =
            case (List.maximum model.domain) of
                Nothing ->
                    0

                Just x ->
                    x

        firstRange =
            case (List.minimum model.range) of
                Nothing ->
                    0

                Just x ->
                    x

        lastRange =
            case (List.maximum model.range) of
                Nothing ->
                    0

                Just x ->
                    x

        a =
            if ((lastDomain - firstDomain) /= 0) && ((lastRange - firstRange) /= 0) then
                (lastRange - firstRange) / (lastDomain - firstDomain)
            else
                1

        b =
            if ((lastDomain - firstDomain) /= 0) && ((lastRange - firstRange) /= 0) then
                firstRange - firstDomain * a
            else
                0

        newLookup =
            Dict.fromList (List.map (\x -> ( x, a * x + b )) model.domain)
    in
        { model | a = a, b = b, lookup = newLookup }



-- Model


type alias Model =
    { category : Category.Category
    , domain : List Float
    , range : List Float
    , lookup : Dict Float Float
    , a : Float
    , b : Float
    }


category : Category
category =
    Linear


defaultDomain : List Float
defaultDomain =
    []


defaultRange : List Float
defaultRange =
    []


defaultLookup : Dict Float Float
defaultLookup =
    Dict.empty


linearScale : Model
linearScale =
    { category = category
    , domain = defaultDomain
    , range = defaultRange
    , lookup = defaultLookup
    , a = 1
    , b = 0
    }
