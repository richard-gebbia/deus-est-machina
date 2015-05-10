module Portrait where

import ClickForm
import Graphics.Collage as Collage
import Graphics.Element as Element
import Interjection exposing (..)
import Signal
import Sprite exposing (..)

-- Model

type alias Model = 
    {
        name: String,
        interjection: Interjection.Model,
        sprite: Sprite
    }

-- Action

type Action = SetInterjection Interjection

-- Update

update : Action -> Model -> Model
update (SetInterjection interjection) model = 
    let action = (Interjection.SetInterjection interjection)
    in
    { model | 
        interjection <- Interjection.update action model.interjection 
    }

-- View

view : Model -> List Collage.Form
view model = 
    [
        Sprite.draw model.sprite,
        Interjection.view model.interjection
    ]

-- Events

type Event = OnClick


onClick : Model -> Signal Event
onClick model =
    let sprite = model.sprite 
        isHovering = 
            ClickForm.rectHitTest 
                sprite.width 
                sprite.height 
                (round sprite.x) 
                (round sprite.y)
    in
    ClickForm.formClick isHovering
    |> Signal.map (always OnClick)
