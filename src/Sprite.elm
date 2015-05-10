module Sprite where

import Graphics.Collage as Collage
import Graphics.Element as Element


type alias Sprite = 
    {
        x: Float,
        y: Float,
        width: Int,
        height: Int,
        imageName: String
    }


draw : Sprite -> Collage.Form
draw sprite = 
    Element.image sprite.width sprite.height sprite.imageName
    |> Collage.toForm
    |> Collage.move (sprite.x, sprite.y)

