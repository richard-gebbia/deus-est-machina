module Textbox where

import Basics exposing (..)
import ClickForm
import Graphics.Collage as Collage
import Graphics.Element as Element
import List
import Sprite exposing (Sprite)
import String
import Text exposing (Text)

-- Model

type alias Model = 
    {
        name: String,
        text: List String,
        background: Sprite,
        portrait: Sprite,
        nameStyle: Text.Style,
        nameX: Float,
        nameY: Float,
        textStyle: Text.Style,
        textX: Float,
        textY: Float,
        lettersPerSecond: Float,
        elapsedTime: Float,
        clickable: Bool
    }


textToShow : Model -> List String
textToShow model = 
    let lettersToShow = model.lettersPerSecond * model.elapsedTime |> round
        inner str (strings, numLetters) = 
            if numLetters >= 0 then
                ([String.left numLetters str] |> List.append strings, numLetters - String.length str)
            else (strings, numLetters)
    in
    List.foldl inner ([], lettersToShow) model.text |> fst


timeToFinish : Model -> Float
timeToFinish model = 
    List.map String.length model.text
    |> List.sum
    |> Basics.toFloat
    |> \x -> x / model.lettersPerSecond

isFinished : Model -> Bool
isFinished model =
    model.elapsedTime >= timeToFinish model

-- Action

type Action = MoveBy (Float, Float)
            | Finish
            | Tick Float

-- Update

type Request = FinishedShowingText


update : Action -> Model -> (Model, Maybe Request)
update action model = 
    case action of
        MoveBy (x, y) ->    
            (
                { model |
                    background <-   
                        { 
                            x = model.background.x + x,
                            y = model.background.y + y,
                            width = model.background.width,
                            height = model.background.height,
                            imageName = model.background.imageName
                        },
                    portrait <-
                        {
                            x = model.portrait.x + x,
                            y = model.portrait.y + y,
                            width = model.portrait.width,
                            height = model.portrait.height,
                            imageName = model.portrait.imageName
                        },
                    nameX <- model.nameX + x,
                    nameY <- model.nameY + y,
                    textX <- model.textX + x,
                    textY <- model.textY + y
                }, 
                Nothing
            )
        Finish -> 
            (
                { model | elapsedTime <- timeToFinish model },
                Just FinishedShowingText
            )
        Tick elapsed -> 
            (
                { model | elapsedTime <- model.elapsedTime + elapsed },
                if model.elapsedTime + elapsed >= timeToFinish model then
                    Just FinishedShowingText
                else
                    Nothing
            )

-- View

formText : Text -> Collage.Form
formText text = 
    let contain (width, height) = Element.container width height
        el = Element.leftAligned text
        (iElWidth, iElHeight) = Element.sizeOf el
        (elWidth, elHeight) = (Basics.toFloat iElWidth, Basics.toFloat iElHeight)
    in
    contain (Element.sizeOf el) Element.topLeft el
    |> Collage.toForm
    |> Collage.move (elWidth / 2, -elHeight / 2)


view : Signal.Address Event -> Model -> List Collage.Form
view address model = 
    let prepend x y = y ++ x
        drawText x y style text = 
            List.intersperse "\n" text
            |> List.foldl prepend ""
            |> Text.fromString
            |> Text.style style
            |> formText
            |> Collage.move (x, y)
        background = 
            if model.clickable then
                ClickForm.spriteButton 
                    model.background 
                    (Signal.message address Click)
            else
                Sprite.draw model.background
    in
    [
        background,
        Sprite.draw model.portrait,
        drawText model.nameX model.nameY model.nameStyle [model.name],
        drawText model.textX model.textY model.textStyle (textToShow model)
    ]

-- Events

type Event = Click
