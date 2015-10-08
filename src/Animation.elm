module Animation where

type alias Animation a = 
    { a |
        duration: Float,
        easing: Float -> Float,
        elapsedTime: Float,
        prevElapsedTime: Float,
        isLooping: Bool
    }


animate : a -> Animation a
animate data =
    { data | duration = 0 }
    |> (\data -> { data | easing = identity })
    |> (\data -> { data | elapsedTime = 0 })
    |> (\data -> { data | prevElapsedTime = 0 })
    |> (\data -> { data | isLooping = False })


linear : Float -> a -> Animation a
linear duration data = 
    animate data
    |> (\animation -> 
        { animation |
            duration <- duration,
            easing <- identity,
            elapsedTime <- 0,
            prevElapsedTime <- 0
        })


curve : Float -> (Float -> Float) -> a -> Animation a
curve duration easing data =
    animate data
    |> (\animation ->
        { animation |
            duration <- duration,
            easing <- easing,
            elapsedTime <- 0,
            prevElapsedTime <- 0
        })


looping : Animation a -> Animation a
looping animation =
    { animation | 
        isLooping <- True
    }


oneShot : Animation a -> Animation a
oneShot animation =
    { animation |
        isLooping <- False
    }


update : Float -> Animation a -> Animation a
update dt animation = 
    let wrapTime : Animation a -> Animation a
        wrapTime anim =
        if anim.isLooping && anim.elapsedTime > anim.duration then
            { anim | elapsedTime <- anim.elapsedTime - anim.duration }
        else
            anim
    in
    { animation |
        elapsedTime <- animation.elapsedTime + dt,
        prevElapsedTime <- animation.elapsedTime
    } |> wrapTime


tByFrame : Animation a -> Float
tByFrame animation =
    let t0 = clamp 0 1 (animation.prevElapsedTime / animation.duration)
        t1 = clamp 0 1 (animation.elapsedTime / animation.duration)
    in
    animation.easing t1 - animation.easing t0


tTotal : Animation a -> Float
tTotal animation =
    clamp 0 1 (animation.elapsedTime / animation.duration)
    |> animation.easing


reset : Animation a -> Animation a
reset animation =
    { animation |
        elapsedTime <- 0,
        prevElapsedTime <- 0
    }


isComplete : Animation a -> Bool
isComplete animation = 
    if animation.isLooping then
        False
    else
        animation.elapsedTime >= animation.duration
