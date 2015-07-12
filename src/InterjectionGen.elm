module InterjectionGen where

import Animation exposing (Animation)
import Interjection

defaultInterjection : Float -> Float -> Interjection.Model
defaultInterjection x y = 
    let fadeFormula x = (cos (x * 2 * pi) + 1) / 2
    in
    {
        x = x,
        y = y,
        width = 40, 
        height = 29,
        interjection = Interjection.Quiet,
        exclamationImageName = "img/Exclamation.png",
        continuationImageName = "img/Ellipsis.png",
        pulseFade = Animation.curve 2 fadeFormula {} |> Animation.looping
    } 