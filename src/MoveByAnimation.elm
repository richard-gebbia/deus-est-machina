module MoveByAnimation where


type alias MoveByAnimation = 
    {
        x: Float,
        y: Float,
        duration: Float,
        easing: Float -> Float,
        elapsedTime: Float,
        prevElapsedTime: Float
    }


linearAnimation : Float -> Float -> Float -> MoveByAnimation
linearAnimation x y duration = 
    {
        x = x,
        y = y,
        duration = duration,
        easing = identity,
        elapsedTime = 0,
        prevElapsedTime = 0
    }


curveAnimation : Float -> Float -> Float -> (Float -> Float) -> MoveByAnimation
curveAnimation x y duration easing =
    {
        x = x,
        y = y,
        duration = duration,
        easing = easing,
        elapsedTime = 0,
        prevElapsedTime = 0
    }


update : MoveByAnimation -> Float -> MoveByAnimation
update animation dt = 
    { animation |
        elapsedTime <- animation.elapsedTime + dt,
        prevElapsedTime <- animation.elapsedTime
    }


moveByFrame : MoveByAnimation -> (Float, Float)
moveByFrame animation =
    let t0 = clamp 0 1 (animation.prevElapsedTime / animation.duration)
        t1 = clamp 0 1 (animation.elapsedTime / animation.duration)
        t = animation.easing t1 - animation.easing t0
    in
    (
        t * animation.x,
        t * animation.y
    )


moveByTotal : MoveByAnimation -> (Float, Float)
moveByTotal animation =
    let t = clamp 0 1 (animation.elapsedTime / animation.duration)
    in
    (
        t * animation.x,
        t * animation.y
    )


reset : MoveByAnimation -> MoveByAnimation
reset animation =
    { animation |
        elapsedTime <- 0,
        prevElapsedTime <- 0
    }


isComplete : MoveByAnimation -> Bool
isComplete animation = 
    animation.elapsedTime >= animation.duration