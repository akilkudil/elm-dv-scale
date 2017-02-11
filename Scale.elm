module Scale exposing (domain, range, scale,getScale)
import List exposing (..)
import Dict exposing (..)


-- update

domain rawData model= 
       let 
            start = case (head rawData) of
                        Nothing -> 0
                        Just x -> x
            stop = case (head (reverse rawData)) of
                        Nothing -> 0
                        Just y -> y
        in
           {model | domain = {start=start, stop=stop, data=rawData}}

insertToDict: Float->Float->Dict Float Float->Dict Float Float
insertToDict k v dict =
             let 
               newMap = Dict.insert k v dict
             in 
               newMap
getScale: Float->Model->Float
getScale dp model = 
             let
                scaledValue = Dict.get dp model.range.data
             in
                case scaledValue of 
                    Nothing -> (model.range.a*dp+model.range.b)
                    Just x -> x 
                       

range limits model=
       let
            start = case (head limits) of
                        Nothing -> 0
                        Just x -> x
            stop = case (head (reverse limits)) of
                        Nothing -> 0
                        Just y -> y         
               
            a = if (model.domain.stop - model.domain.start) > 0 
                    then
                      (stop-start)/(model.domain.stop - model.domain.start)
                    else
                      1
            b = if (model.domain.stop - model.domain.start) > 0
                    then
                      start - model.domain.start*a
                    else
                      0
            emptyDict = Dict.empty
            
            scaledData = if (a/=0 && b/=0)
                            then
                               Dict.fromList (List.map (\x -> (x,a*x+b)) model.domain.data)
                            else
                               emptyDict
            
            
       in
           {model | range = {start=start, stop=stop,a=a, b=b, data=scaledData}}


-- Model 

type alias Model =
          {
            category:Category, 
            domain:Domain,
            range:Range
          }           
type alias Domain =
          {
            start:Float,
            stop:Float,
            data: List Float
          }
type alias Range = 
          {
            start:Float,
            stop:Float,
            a:Float,
            b:Float,


            data: Dict Float Float
          }
defaultCategory = Linear

defaultDomain =
            {
             start=0,
             stop=0,
             data = []
            }

defaultRange = 
            {
              start=0,
              stop=0,
              a=1,
              b=0,
              data=Dict.empty
            }


type Category = Linear | Ordinal

scale : Model
scale = 
        {
            category = defaultCategory,
            domain = defaultDomain,
            range = defaultRange
        }

