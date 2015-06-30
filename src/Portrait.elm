module Portrait where

import ClickForm
import Graphics.Collage as Collage
import Graphics.Element as Element
import List
import Maybe
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

type Action 
    = SetResponse (List String) String
    | Quiet

-- Update

update : Action -> Model -> Model
update action model = 
    let determineInterjection previous name =
            if name == previous then
                Interjection.Continuation
            else
                Interjection.Exclamation
        interjection names previous = 
            List.filter (\name -> name == model.name) names
            |> List.head
            |> Maybe.map (determineInterjection previous)
            |> Maybe.withDefault Interjection.Quiet
        iAction names previous = 
            (Interjection.SetInterjection (interjection names previous))
    in
    case action of
        SetResponse names previous ->
            { model | 
                interjection <- 
                    Interjection.update 
                        (iAction names previous) 
                        model.interjection 
            }

        Quiet -> 
            { model |
                interjection <- 
                    Interjection.update 
                        (Interjection.SetInterjection Interjection.Quiet)
                        model.interjection
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
