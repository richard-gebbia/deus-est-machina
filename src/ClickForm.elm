module ClickForm where

import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Input as Input
import Signal
import Sprite exposing (Sprite)

spriteButton : Sprite -> Signal.Message -> Collage.Form
spriteButton sprite msg =
    Input.customButton msg
        (Element.image sprite.width sprite.height sprite.imageName)
        (Element.image sprite.width sprite.height sprite.imageName)
        (Element.image sprite.width sprite.height sprite.imageName)
    |> Collage.toForm
    |> Collage.move (sprite.x, sprite.y)
