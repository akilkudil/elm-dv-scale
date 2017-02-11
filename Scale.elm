module Scale exposing (..)

import Category exposing (Category(..))
import LinearScale exposing (..)


--update


linearScale : Scale
linearScale =
    { scale
        | category = Linear
        , linear = Just LinearScale.linearScale
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


getScaledValue data scale =
    case scale.category of
        Linear ->
            case scale.linear of
                Nothing ->
                    -999999

                Just x ->
                    LinearScale.getScaledValue data x

        _ ->
            -999999



--Model


type alias Scale =
    { category : Category
    , linear : Maybe LinearScale.Model
    }


scale : Scale
scale =
    { category = None
    , linear = Nothing
    }
