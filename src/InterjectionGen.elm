module InterjectionGen where

import Interjection

defaultInterjection : Float -> Float -> Interjection.Model
defaultInterjection x y = 
    Interjection.Model x y 62 45 Interjection.Quiet "img/exclamation.png" "img/dotdotdot.png"