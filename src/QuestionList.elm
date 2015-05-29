module QuestionList where

import Debug
import Graphics.Collage as Collage
import List
import Textbox

-- Model

type alias Model = 
    {
        questions: List Textbox.Model,
        firstQuestionPosition: (Float, Float),
        questionStride: (Float, Float),
        questionFrame: List String -> (Float, Float) -> Textbox.Model,
        questionChildren: List (List String)
    }

-- Action

type Action 
    = ShowQuestions (List (List String)) (List (List String))
    | Hide

-- Update

makeQuestions : Model -> List (List String) -> List Textbox.Model
makeQuestions model questions =
    let makeQuestion text position =
            model.questionFrame text position
            |> Textbox.update (Textbox.Finish)
            |> fst
        position (x, y) (vx, vy) t =
            (x + vx * t, y + vy * t)
        positions =
            List.map 
                (position model.firstQuestionPosition model.questionStride)
                (List.map toFloat [0..(List.length questions - 1)])
    in
    List.map2 makeQuestion questions positions


update : Action -> Model -> Model
update action model =
    case action of
        ShowQuestions questions children ->
            { model |
                questions <- makeQuestions model questions,
                questionChildren <- children
            }
        Hide ->
            { model |
                questions <- []
            }

-- View

view : Signal.Address Event -> Model -> List Collage.Form
view address model =
    let viewQuestion question children =
            Textbox.view 
                (Signal.forwardTo address (always (ChooseQuestion children)))
                question
    in
    List.map2 viewQuestion model.questions model.questionChildren
    |> List.concat

-- Events

type Event
    = ChooseQuestion (List String)
