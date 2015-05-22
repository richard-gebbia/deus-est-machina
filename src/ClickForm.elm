module ClickForm where

import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Input as Input
import Mouse
import Signal
import Sprite exposing (Sprite)
import Window

type alias HitTest = (Int, Int) -> Bool


formClick : HitTest -> Signal ()
formClick isHovering =
    Signal.sampleOn Mouse.clicks Mouse.position     -- every time you click, grab the mouse position
    |> Signal.map2 toWorldSpace Window.dimensions   -- transform it to world space 
    |> Signal.map isHovering                        -- map the mouse position to a bool (whether it's over the form)
    |> Signal.filter identity False                 -- keep the signal if the bool is true (defaults to false)
    |> Signal.map (always ())                       -- once we've kept it, we don't care about its value


rectHitTest : Int -> Int -> Int -> Int -> (Int, Int) -> Bool
rectHitTest width height x y (ox, oy) =
    ox < x + width  // 2  &&
    ox > x - width  // 2  &&
    oy < y + height // 2  &&
    oy > y - height // 2


toWorldSpace : (Int, Int) -> (Int, Int) -> (Int, Int)
toWorldSpace (width, height) (x, y) = 
    (x - width // 2, (height // 2) - y)


spriteButton : Sprite -> Signal.Message -> Collage.Form
spriteButton sprite msg =
    Input.customButton msg
        (Element.image sprite.width sprite.height sprite.imageName)
        (Element.image sprite.width sprite.height sprite.imageName)
        (Element.image sprite.width sprite.height sprite.imageName)
    |> Collage.toForm
    |> Collage.move (sprite.x, sprite.y)
