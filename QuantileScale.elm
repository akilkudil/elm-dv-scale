module QuantileScale exposing (domain, derivedDomain, range, quantileScale, lookupDomain, lookupRange, setLookup, transform)

import Category exposing (Category(..))
import List exposing (..)


-- update


transform func model =
    func model


domain : List Float -> Model a -> Model a
domain list model =
    { model | domain = list } |> derivedDomain list


derivedDomain : List Float -> Model a -> Model a
derivedDomain list model =
    { model | derivedDomain = List.indexedMap (,) (List.sort list) }


range : List a -> Model a -> Model a
range list model =
    { model | range = list } |> derivedRange


derivedRange : Model a -> Model a
derivedRange model =
    { model | derivedRange = List.indexedMap (,) model.range, n_quantile = (List.length model.range) - 1 }


isEven : Int -> Bool
isEven x =
    (x % 2 == 0)


rankEq domainSize nquantile =
    domainSize / nquantile


rank model =
    let
        nquantile =
            toFloat (model.n_quantile)

        domainSize =
            toFloat (List.length model.derivedDomain)
    in
        rankEq domainSize nquantile


setLookup : Model a -> Model a
setLookup model =
    let
        rank1 =
            rank model
    in
        { model
            | lookup =
                List.map
                    (\( w, _ ) ->
                        ( w, ceiling (toFloat (w) * (rank model)) )
                    )
                    model.derivedRange
        }


getDomainX w model =
    List.filter (\( a, b ) -> (b == w)) model.derivedDomain |> List.map (\( x, y ) -> x)


getDomainY w model =
    List.filter (\( a, b ) -> (a == w)) model.derivedDomain |> List.map (\( x, y ) -> y)


getRangeX w model =
    List.filter (\( a, b ) -> (b == w)) model.derivedRange |> List.map (\( x, y ) -> x)


getRangeY w model =
    List.filter (\( a, b ) -> (a == w)) model.derivedRange |> List.map (\( x, y ) -> y)


getLookupX : Int -> Model a -> List Int
getLookupX w model =
    List.filter (\( a, b ) -> (b == w)) model.lookup |> List.map (\( x, y ) -> x)


getLookupY : Int -> Model a -> List Int
getLookupY w model =
    List.filter (\( a, b ) -> (a == w)) model.lookup |> List.map (\( x, y ) -> y)


lookupRange : Float -> Model a -> List a
lookupRange dp model =
    let
        domainX =
            getDomainX dp model

        lookupX =
            case domainX of
                [ x ] ->
                    getLookupX x model

                _ ->
                    []

        rangeY =
            case lookupX of
                [ x ] ->
                    getRangeY x model

                _ ->
                    []
    in
        rangeY


lookupDomain : a -> Model a -> List Float
lookupDomain rp model =
    let
        rangeX =
            getRangeX rp model

        lookupY =
            case rangeX of
                [ x ] ->
                    getLookupY x model

                _ ->
                    []

        domainY =
            case lookupY of
                [ x ] ->
                    getDomainY x model

                _ ->
                    []
    in
        domainY



--Model


type alias Model a =
    { category : Category
    , domain : List Float
    , derivedDomain : List ( Int, Float )
    , range : List a
    , derivedRange : List ( Int, a )
    , lookup : List ( Int, Int )
    , reverseLookup : List ( Int, Int )
    , n_quantile : Int
    }


quantileScale : Model a
quantileScale =
    { category = Quantile
    , domain = []
    , derivedDomain = []
    , range = []
    , derivedRange = []
    , lookup = []
    , reverseLookup = []
    , n_quantile = 2
    }
