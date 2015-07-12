module PortraitBox where

import Graphics.Collage as Collage
import Graphics.Element as Element
import List exposing ((::))
import Portrait
import Signal
import Sprite exposing (Sprite)

-- Model

type alias Model = 
    {
        portraits: List Portrait.Model,
        previous: String
    }


names : Model -> List String
names model =
    List.map .name model.portraits

-- Action

type Action 
    = SetResponders (List String)
    | LetThemSpeak String
    | ThePlayerIsSpeaking
    | Tick Float

-- Update

update : Action -> Model -> Model
update action model = 
    let actions names = 
            List.repeat
                (List.length model.portraits)
                (Portrait.SetResponse names model.previous)
        quiet = 
            Portrait.update Portrait.Quiet
        tick dt =
            Portrait.update (Portrait.Tick dt)
    in
    case action of 
        SetResponders names ->
            { model |
                portraits <- 
                    List.map2 Portrait.update (actions names) model.portraits
            }

        LetThemSpeak speakerName ->
            { model |
                portraits <- List.map quiet model.portraits,
                previous <- speakerName
            }

        ThePlayerIsSpeaking -> 
            { model |
                portraits <- List.map quiet model.portraits,
                previous <- ""
            }

        Tick dt ->
            { model |
                portraits <- List.map (tick dt) model.portraits
            }


-- View

view : Signal.Address Event -> Model -> List Collage.Form
view address model = 
    let viewPortrait model = 
            Portrait.view 
                (Signal.forwardTo address 
                    (always (OnPortraitClick model.name)))
                model
    in
    List.map viewPortrait model.portraits |> List.concat


-- Events

type Event = OnPortraitClick String
