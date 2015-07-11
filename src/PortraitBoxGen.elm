module PortraitBoxGen where

import PortraitBox
import PortraitGen
import Sprite exposing (Sprite)

genPortraitBox : Float -> Float -> PortraitBox.Model
genPortraitBox x y =
    {
        portraits =
            [
                PortraitGen.sophie      (x - 240) y,
                PortraitGen.gavin       (x - 82) y,
                PortraitGen.ava         (x + 82) y,
                PortraitGen.sebastian   (x + 240) y
            ],
        previous = ""
    }
