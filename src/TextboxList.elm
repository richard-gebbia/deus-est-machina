module TextboxList where

import Dict exposing (Dict)
import Graphics.Collage as Collage
import List exposing ((::))
import Maybe exposing (andThen)
import MoveByAnimation exposing (MoveByAnimation)
import Signal
import Textbox
import TextboxGen

-- Model

type alias Model = 
    {
        textboxes: List Textbox.Model,
        animation: MoveByAnimation,
        enterPosition: (Float, Float),
        textboxFrames: Dict String ((List String) -> (Float, Float) -> Textbox.Model),
        hidden: Bool
    }

isReadyForNewTextboxes : Model -> Bool
isReadyForNewTextboxes model =
    List.head model.textboxes
    |> Maybe.map Textbox.isFinished
    |> Maybe.withDefault True

-- Action

type Action = AddTextbox String (List String)
            | FinishCurrentTextbox
            | Tick Float

-- Update

type Request = ReadyForNewTextboxes


maybeGuard : Bool -> a -> Maybe a
maybeGuard b m =
    if b then Just m else Nothing


bubbleTextboxRequest : Maybe Textbox.Request -> Maybe Request
bubbleTextboxRequest request =
    case request of
        (Just (Textbox.FinishedShowingText)) ->
            Just ReadyForNewTextboxes

        _ -> 
            Nothing


addTextbox : String -> List String -> Model -> (Model, Maybe Request)
addTextbox name text model =
    (
        { model |
            animation <- MoveByAnimation.reset model.animation,
            textboxes <- Dict.get name model.textboxFrames
                         |> Maybe.map (\frame -> frame text model.enterPosition)
                         |> Maybe.map (\tb -> tb :: model.textboxes)
                         |> Maybe.withDefault model.textboxes
        },
        Nothing
    )


onUpdateTextbox : Model -> (Textbox.Model, Maybe Textbox.Request) -> (Model, Maybe Request)
onUpdateTextbox model (textbox, request) =
    (
        { model |
            textboxes <- textbox :: List.drop 1 model.textboxes
        },
        bubbleTextboxRequest request
    )    


finishCurrentTextbox : Model -> (Model, Maybe Request)
finishCurrentTextbox model =
    List.head model.textboxes
    `andThen` (maybeGuard <| MoveByAnimation.isComplete model.animation)
    |> Maybe.map (Textbox.update Textbox.Finish)
    |> Maybe.map (onUpdateTextbox model)
    |> Maybe.withDefault (model, Nothing)


moveTextboxes : Float -> Model -> (Model, Maybe Request)
moveTextboxes dt model = 
    let updatedAnim = MoveByAnimation.update model.animation dt
        moveBy = Textbox.MoveBy (MoveByAnimation.moveByFrame updatedAnim)
    in
    (
        { model |
            animation <- updatedAnim,
            textboxes <- List.map (Textbox.update moveBy >> fst) model.textboxes
        },
        Nothing
    )


tick : Float -> Model -> (Model, Maybe Request)
tick dt model = 
    List.head model.textboxes
    `andThen` maybeGuard (MoveByAnimation.isComplete model.animation)
    |> Maybe.map (Textbox.update (Textbox.Tick dt))
    |> Maybe.map (onUpdateTextbox model) 
    |> Maybe.withDefault (moveTextboxes dt model)


update : Action -> Model -> (Model, Maybe Request)
update action model = 
    case action of 
        AddTextbox name text -> 
            addTextbox name text model

        FinishCurrentTextbox -> 
            finishCurrentTextbox model

        Tick dt -> 
            tick dt model
            
-- View

view : Model -> List Collage.Form
view model =
    let address = nullMailbox.address
    in
    if model.hidden then []
    else
        List.concatMap 
            (Textbox.view (Signal.forwardTo address (always ())))
            model.textboxes

-- Events

nullMailbox : Signal.Mailbox ()
nullMailbox = Signal.mailbox ()
