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

view : Model -> List Collage.Form
view model = 
    let bgSprite = Sprite.draw model.background
        portraits = List.map Portrait.view model.portraits |> List.concat
    in
    bgSprite :: portraits

-- Events

type Event = OnPortraitClick String


onPortraitClick : Model -> Signal Event
onPortraitClick model =
    let portraitSignal portrait = 
            Portrait.onClick portrait
            |> Signal.map (always (OnPortraitClick portrait.name))
    in
    List.map portraitSignal model.portraits
    |> Signal.mergeMany