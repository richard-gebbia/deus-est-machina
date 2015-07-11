module InterjectionGen where

import Interjection

defaultInterjection : Float -> Float -> Interjection.Model
defaultInterjection x y = 
    Interjection.Model x y 40 29 Interjection.Quiet "img/Exclamation.png" "img/Ellipsis.png"