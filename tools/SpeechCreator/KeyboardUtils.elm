module KeyboardUtils where

import Char
import Keyboard


type alias PressState =
    { current: Bool
    , previous: Bool
    }


isTriggered : PressState -> Bool
isTriggered pressState =
    pressState.current && (not pressState.previous)


keyPresses : Char.KeyCode -> Signal ()
keyPresses keyCode =
    let init : PressState
        init = 
            { current = False
            , previous = False
            }

        update : Bool -> PressState -> PressState
        update current pressState =
            PressState current pressState.current
    in
    Signal.foldp update init (Keyboard.isDown keyCode)
    |> Signal.filter isTriggered init
    |> Signal.map (always ())