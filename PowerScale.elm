module PowerScale exposing (domain, range, powerScale, lookupRange, lookupDomain, setLookup, transform)

import LinearScale exposing (..)
import Category exposing (Category(..))
import List exposing (..)


transform func model =
    func model


baseTransformDef pow baseModel =
    { baseModel | derivedDomain = List.map (\x -> x ^ pow) baseModel.derivedDomain }


applyBaseTransform model =
    { model | baseScale = baseTransformDef model.exp model.baseScale }


domain list model =
    { model | baseScale = LinearScale.domain list model.baseScale } |> applyBaseTransform


range list model =
    { model | baseScale = LinearScale.range list model.baseScale }


lookupRange dp model =
    LinearScale.lookupRange (dp ^ (model.exp)) model.baseScale


lookupDomain rp model =
    (LinearScale.lookupDomain rp model.baseScale) ^ (1 / model.exp)


setLookup model =
    { model | baseScale = LinearScale.setLookup model.baseScale }


powerScale k =
    { category = Power
    , baseScale = linearScale
    , exp = k
    }
