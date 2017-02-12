module Scale exposing (message)

import Category exposing (Category(..))
import LinearScale exposing (..)
import OrdinalScale exposing (..)


--update


message =
    "This is deprecated. This was attempt as an abstraction layer and it does not work"


linearScale : Scale
linearScale =
    { scale
        | category = Linear
        , linear = Just LinearScale.linearScale
    }


ordinalScale : Scale
ordinalScale =
    { scale
        | category = Ordinal
        , ordinal = Just OrdinalScale.ordinalScale
    }


domain data scale =
    case scale.category of
        Linear ->
            case scale.linear of
                Nothing ->
                    scale

                Just x ->
                    { scale
                        | linear = Just (LinearScale.domain data x)
                    }

        Ordinal ->
            case scale.ordinal of
                Nothing ->
                    scale

                Just x ->
                    { scale | ordinal = Just (OrdinalScale.domain data x) }

        --scale
        _ ->
            scale


range data scale =
    case scale.category of
        Linear ->
            case scale.linear of
                Nothing ->
                    scale

                Just x ->
                    { scale
                        | linear = Just (LinearScale.range data x)
                    }

        _ ->
            scale


lookupRange data scale =
    case scale.category of
        Linear ->
            case scale.linear of
                Nothing ->
                    -999999

                Just x ->
                    LinearScale.lookupRange data x

        Ordinal ->
            case scale.ordinal of
                Nothing ->
                    -99999

                Just x ->
                    OrdinalScale.lookupRange data x

        _ ->
            -999999



--Model


type alias Scale =
    { category : Category.Category
    , linear : Maybe LinearScale.Model
    , ordinal : Maybe OrdinalScale.Model
    }


scale : Scale
scale =
    { category = None
    , linear = Nothing
    , ordinal = Nothing
    }
