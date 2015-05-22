module PortraitBox where

import Graphics.Collage as Collage
import Graphics.Element as Element
import Interjection exposing (..)
import List exposing ((::))
import Portrait
import Signal
import Sprite exposing (Sprite)

-- Model

type alias Model = 
    {
        background: Sprite,
        portraits: List Portrait.Model
    }


names : Model -> List String
names model =
    List.map .name model.portraits

-- Action

type Action 
    = SetInterjections (List Interjection)
    | LetThemSpeak

-- Update

update : Action -> Model -> Model
update action model = 
    let actions interjections = 
            List.map Portrait.SetInterjection interjections
        quiet = 
            Portrait.update (Portrait.SetInterjection Interjection.Quiet)
    in
    case action of 
        SetInterjections interjections ->
            { model |
                portraits <- 
                    List.map2 Portrait.update (actions interjections) model.portraits
            }

        LetThemSpeak ->
            { model |
                portraits <-
                    List.map quiet model.portraits
            }

-- View

view : Signal.Address Event -> Model -> List Collage.Form
view address model = 
    let bgSprite = Sprite.draw model.background
        viewPortrait model = 
            Portrait.view 
                (Signal.forwardTo address 
                    (always (OnPortraitClick model.name)))
                model
        portraits = List.map viewPortrait model.portraits |> List.concat
    in
    bgSprite :: portraits

-- Events

type Event = OnPortraitClick String
