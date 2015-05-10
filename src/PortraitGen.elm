module PortraitGen where

import Debug
import Interjection
import InterjectionGen
import Portrait
import Sprite exposing (Sprite)

model : Float -> Float -> String -> Portrait.Model
model x y name = 
    let imgPath = "img/" ++ name ++ "Portrait.png"
    in
    Portrait.Model 
        name 
        (InterjectionGen.defaultInterjection (x + 32) (y + 40))
        (Sprite x y 155 155 imgPath)


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
