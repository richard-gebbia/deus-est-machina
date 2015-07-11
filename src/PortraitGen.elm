module PortraitGen where

import Debug
import Interjection
import InterjectionGen
import Portrait
import Sprite exposing (Sprite)

model : Float -> Float -> String -> Portrait.Model
model x y name = 
    let imgPath = "img/" ++ name ++ ".png"
        framePath = "img/" ++ name ++ "Frame.png"
    in
    Portrait.Model 
        name 
        (InterjectionGen.defaultInterjection (x + 21) (y + 25))
        (Sprite (x - 10) (y - 11) 64 64 imgPath)
        (Sprite x y 100 100 framePath)


sophie : Float -> Float -> Portrait.Model
sophie x y = 
    model x y "Sophie"


gavin : Float -> Float -> Portrait.Model
gavin x y = 
    model x y "Gavin"


ava : Float -> Float -> Portrait.Model
ava x y = 
    model x y "Ava"
    

sebastian : Float -> Float -> Portrait.Model
sebastian x y = 
    model x y "Sebastian"
