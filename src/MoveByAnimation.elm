module MoveByAnimation where

import Animation exposing (Animation)


type alias MoveByAnimation = Animation { x: Float, y: Float }


linear : Float -> Float -> Float -> MoveByAnimation
linear x y duration = 
    Animation.linear duration { x = x, y = y }


curve : Float -> Float -> Float -> (Float -> Float) -> MoveByAnimation
curve x y duration easing =
    Animation.curve duration easing { x = x, y = y }


moveByFrame : MoveByAnimation -> (Float, Float)
moveByFrame animation =
    let t = Animation.tByFrame animation
    in
    (
        t * animation.x,
        t * animation.y
    )


moveByTotal : MoveByAnimation -> (Float, Float)
moveByTotal animation =
    let t = Animation.tTotal animation
    in
    (
        t * animation.x,
        t * animation.y
    )
