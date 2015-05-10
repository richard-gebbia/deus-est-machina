module Interjection where

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
        continuationImageName: String
    }

-- Action

type Action = SetInterjection Interjection

-- Update

update : Action -> Model -> Model
update (SetInterjection interjection) model = 
    { model | interjection <- interjection }

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
    in
    Collage.toForm element
    |> Collage.move (model.x, model.y)

-- Events
