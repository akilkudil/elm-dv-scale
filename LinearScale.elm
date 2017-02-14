module LinearScale exposing (domain, derivedDomain, range, linearScale, lookupRange, lookupDomain, setLookup, transform)

import Category exposing (Category(..))
import Scale exposing (min, max)
import List exposing (..)


-- update


transform func model =
    func model


domain : List Float -> Model -> Model
domain list model =
    { model | domain = [ (Scale.min list), (Scale.max list) ] } |> derivedDomain list


derivedDomain : List Float -> Model -> Model
derivedDomain list model =
    { model | derivedDomain = [ (Scale.min list), (Scale.max list) ] }


range : List Float -> Model -> Model
range list model =
    { model | range = list } |> setLookup


lookupRange : Float -> Model -> Float
lookupRange dp model =
    model.a * dp + model.b


lookupDomain : Float -> Model -> Float
lookupDomain rp model =
    (rp - model.b) / model.a


setLookup model =
    let
        firstDomain =
            Scale.min model.derivedDomain

        lastDomain =
            Scale.max model.derivedDomain

        firstRange =
            Scale.min model.range

        lastRange =
            Scale.max model.range

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
    in
        { model | a = a, b = b }



-- Model


type alias Model =
    { category : Category
    , domain : List Float
    , derivedDomain : List Float
    , range : List Float
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


linearScale : Model
linearScale =
    { category = category
    , domain = defaultDomain
    , derivedDomain = defaultDomain
    , range = defaultRange
    , a = 1
    , b = 0
    }
