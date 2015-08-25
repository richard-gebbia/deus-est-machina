module ArrayUtils where

import Array exposing (Array)
import Maybe


update : Int -> (a -> a) -> Array a -> Maybe (Array a)
update index f array =
    Array.get index array
    |> Maybe.map f
    |> Maybe.map (\x -> Array.set index x array)