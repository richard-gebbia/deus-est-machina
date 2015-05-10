module PortraitBoxGen where

import PortraitBox
import PortraitGen
import Sprite exposing (Sprite)

genPortraitBox : Float -> Float -> PortraitBox.Model
genPortraitBox x y =
    PortraitBox.Model (Sprite x y 1024 168 "img/PortraitHolder.png")
    [
        PortraitGen.sophie      (x - 384.5) y,
        PortraitGen.gavin       (x - 131.5) y,
        PortraitGen.ava         (x + 131.5) y,
        PortraitGen.sebastian   (x + 384.5) y
    ]
