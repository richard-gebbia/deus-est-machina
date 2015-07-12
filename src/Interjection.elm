module Interjection where

import Animation exposing (Animation)
import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)

-- Model

type Interjection = Exclamation | Continuation | Quiet

type alias Model = 
    {
        x: Float,
        y: Float,
        width: Int,
        height: Int,
        interjection: Interjection,
        exclamationImageName: String,
        continuationImageName: String,
        pulseFade: Animation {}
    }

-- Action

type Action 
    = SetInterjection Interjection
    | Tick Float

-- Update

update : Action -> Model -> Model
update action model = 
    case action of
        SetInterjection interjection ->
            { model | interjection <- interjection }

        Tick dt ->
            { model | pulseFade <- Animation.update dt model.pulseFade }

-- View

viewSprite : String -> Model -> Element
viewSprite imageName model =
    Element.image 
        model.width
        model.height
        imageName


view : Model -> Collage.Form
view model =
    let element = 
            case model.interjection of
                Exclamation -> 
                    viewSprite model.exclamationImageName model

                Continuation -> 
                    viewSprite model.continuationImageName model

                Quiet -> 
                    Element.empty

        opacity = 
            Animation.tTotal model.pulseFade
    in
    Element.opacity opacity element
    |> Collage.toForm
    |> Collage.move (model.x, model.y)

-- Events
