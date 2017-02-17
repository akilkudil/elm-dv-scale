module QuantileScale exposing (domain, range, quantileScale, lookupDomain, lookupRange, setLookup, transform)

import Category exposing (Category(..))
import List exposing (..)
import Dict exposing (..)

-- update


transform func model =
    func model


domain : List Float -> Model comparable -> Model comparable
domain list model =
    { model | domain = List.sort list } 




range : List comparable -> Model comparable -> Model comparable
range list model =
    { model | range = list} |> calculateN


calculateN model =
        if not (List.isEmpty model.range) then
            {model | n_quantile = (List.length model.range) - 1 }
        else
            model
            

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
            toFloat (List.length model.domain)
    in
        rankEq domainSize nquantile
            
quartile: Int-> comparable->Float->Dict Int Float->Model comparable->Quartile comparable
quartile index rangeItem rank domain model=
    let 
         nquantile = 
             model.n_quantile
         qindex =
             ceiling( rank*(toFloat index))
         divby2 x = x/2 
         qvalue=
             if index==nquantile then
                   Dict.get ((List.length model.domain)-1) domain
             else
                   if (toFloat index) == ((toFloat nquantile)/2) then 
                        Maybe.map (divby2) 
                             (
                                 Maybe.map2 (+) (Dict.get qindex domain) (Dict.get (qindex+1) domain)
                             )
                   else
                        Dict.get qindex domain
    in
         {
                index= index
              , rangeItem = rangeItem
              , qvalue = qvalue
              , subdomain = []
         }

        
setLookup : Model comparable -> Model comparable
setLookup model =
      let 
        rank1 = 
            rank model


        derivedDomain = 
              Dict.fromList (List.indexedMap (,) model.domain )

        reverseLookupList =
            List.map (\(x,y) ->
                           let
                              qrecord = quartile x y rank1 derivedDomain model
                              q = ceiling ((toFloat x)*rank1) 
                           in
                              
                                 (q, qrecord)
                                
                      ) (List.indexedMap (,) model.range)

        
        lookupList = 
            List.map (\(x,y) ->
                           let
                              qrecord = quartile x y rank1 derivedDomain model 
                           in
                                 (y, qrecord)

                      ) (List.indexedMap (,) model.range)


        lookup = Dict.fromList lookupList
        reverseLookup = Dict.fromList reverseLookupList
     in
        { model | lookup = lookup, reverseLookup = reverseLookup}

lookupRange : Float -> Model comparable -> Maybe String
lookupRange dp model =
    let
        rangeItem = Just "not completed"
    in
        rangeItem


--lookupDomain : comparable -> Model comparable -> Maybe (Quartile  comparable)
lookupDomain : comparable -> Model comparable -> Maybe Int

lookupDomain rp model =
    let
        quartile = 
             Dict.get rp model.lookup
        index =
             Maybe.map .index quartile  
        prevIndex =
             Maybe.map2 (-) index (Just 1)

        qvalue =
             Maybe.map .qvalue quartile
        qvaluePrev =
             case prevIndex of
                Nothing ->
                    Nothing
                Just x ->
                    Maybe.map .qvalue (Dict.get x  model.reverseLookup)
        
        
        qrange =
            case (qvalue,qvaluePrev)  of
                (Nothing,Nothing) ->
                    []
                (Just x, Nothing) ->
                    [x]
                (Nothing, Just y) ->
                    []
                (Just x, Just y) ->
                    [x,y]
    in
       prevIndex         
{-    in
        case quartile of
            Nothing->
               Nothing
            Just x ->
               Just { x | subdomain = qrange }
-}


--Model
type alias Quartile comparable = 
    {
       index : Int
     , rangeItem : comparable
     , qvalue : Maybe Float
     , subdomain : List (Maybe Float)
    }

type alias Model comparable =
    { category : Category
    , domain : List Float
    , range : List comparable
    , lookup : Dict comparable (Quartile comparable)
    , reverseLookup : Dict Int (Quartile comparable)
    , n_quantile : Int
    , step : Int
    }


quantileScale : Model comparable
quantileScale =
    { category = Quantile
    , domain = []
    , range = []
    , lookup = Dict.empty
    , reverseLookup = Dict.empty
    , n_quantile = 2
    , step = 1
    }
