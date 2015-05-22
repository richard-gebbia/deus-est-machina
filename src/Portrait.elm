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

view : Signal.Address Event -> Model -> List Collage.Form
view address model = 
    let spriteView = 
        case model.interjection.interjection of
            Interjection.Quiet ->
                Sprite.draw model.sprite

            _ ->
                ClickForm.spriteButton 
                    model.sprite 
                    (Signal.message address Click)
    in
    [
        spriteView,
        Interjection.view model.interjection
    ]

-- Events

type Event = Click
