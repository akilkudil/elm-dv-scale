module OrdinalScale exposing (domain, range, ordinalScale, lookupRange, setLookup)

import Category exposing (Category(..))
import List exposing (..)
import Dict exposing (..)


--update


domain : List String -> Model -> Model
domain list model =
    { model | domain = list }


range : List Int -> Model -> Model
range list model =
    { model | range = list } |> setLookup


deriveRange : Model -> List Int
deriveRange model =
    case model.range of
        [] ->
            List.range 1 (List.length model.domain)

        _ ->
            model.range


setLookup : Model -> Model
setLookup model =
    let
        newRange =
            deriveRange model
    in
        { model | lookup = Dict.fromList (List.map2 (,) model.domain newRange) }


lookupRange : String -> Model -> Int
lookupRange dp model =
    let
        rangeValue =
            Dict.get dp model.lookup
    in
        case rangeValue of
            Nothing ->
                -99999

            Just x ->
                x



--Model


type alias Model =
    { category : Category
    , domain : List String
    , range : List Int
    , lookup : Dict String Int
    }


category : Category
category =
    Ordinal


defaultDomain : List String
defaultDomain =
    []


defaultRange : List Int
defaultRange =
    []


defaultLookup : Dict String Int
defaultLookup =
    Dict.empty


ordinalScale : Model
ordinalScale =
    { category = category
    , domain = defaultDomain
    , range = defaultRange
    , lookup = defaultLookup
    }
