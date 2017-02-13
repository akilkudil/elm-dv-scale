module PowerScale exposing (domain, range, powerScale, lookupRange, lookupDomain, setLookup)

import LinearScale exposing (..)
import List exposing (..)


domain list model =
    let
        firstDomain =
            case (List.minimum list) of
                Nothing ->
                    0

                Just x ->
                    x ^ model.exp

        lastDomain =
            case (List.maximum list) of
                Nothing ->
                    0

                Just x ->
                    x ^ model.exp
    in
        { model | baseScale = (LinearScale.domain [ firstDomain, lastDomain ] model.baseScale) }


range list model =
    { model | baseScale = LinearScale.range list model.baseScale }


lookupRange dp model =
    LinearScale.lookupRange (dp ^ (model.exp)) model.baseScale


lookupDomain rp model =
    (LinearScale.lookupDomain rp model.baseScale) ^ (1 / model.exp)


setLookup model =
    { model | baseScale = LinearScale.setLookup model.baseScale }


powerScale k =
    { baseScale = linearScale
    , exp = k
    }
