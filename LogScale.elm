module LogScale exposing (domain, range, logScale, lookupRange, lookupDomain, setLookup, transform)

import LinearScale exposing (..)
import Category exposing (Category(..))
import List exposing (..)


transform func model =
    func model


baseTransformDef logbase baseModel =
    { baseModel | derivedDomain = List.map (\x -> logBase logbase x) baseModel.derivedDomain }


applyBaseTransform model =
    { model | baseScale = baseTransformDef model.base model.baseScale }


domain list model =
    { model | baseScale = LinearScale.domain list model.baseScale } |> applyBaseTransform


range list model =
    { model | baseScale = LinearScale.range list model.baseScale }


lookupRange dp model =
    LinearScale.lookupRange (logBase model.base dp) model.baseScale


lookupDomain rp model =
    model.base ^ (LinearScale.lookupDomain rp model.baseScale)


setLookup model =
    { model | baseScale = LinearScale.setLookup model.baseScale }


logScale k =
    { category = Log
    , baseScale = linearScale
    , base = k
    }
