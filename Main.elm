import Html exposing (..)
import Scale exposing (..)
--import Svg exposing (..)

scale3 = scale |> domain [1,7] |> range [1,5]

scale = Scale.scale
domain = Scale.domain
range = Scale.range



main = text(toString(scale |> getScale 5 ))
