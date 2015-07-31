module Questions where

import Array exposing (Array)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlUtils
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe
import Question

-- Model 

type alias Model =
    { questions: Array Question.Model
    }


fromJson : Decode.Decoder Model
fromJson =
    Decode.list Question.fromJson
    |> Decode.map Array.fromList
    |> Decode.map Model


-- Action

type Action
    = ModifyQuestion Int Question.Action
    | NewQuestion
    | DeleteQuestion Int


-- Update

update : Action -> Model -> Model
update action model =
    case action of
        ModifyQuestion questionIndex qAction ->
            { model |
                questions <-
                    Array.get questionIndex model.questions
                    |> Maybe.map (Question.update qAction)
                    |> Maybe.map (\question -> 
                        Array.set questionIndex question model.questions)
                    |> Maybe.withDefault model.questions
            }

        NewQuestion ->
            { model |
                questions <-
                    Array.push Question.init model.questions
            }

        DeleteQuestion questionIndex ->
            { model |
                questions <-
                    Array.append
                        (Array.slice 0 questionIndex model.questions)
                        (Array.slice 
                            (questionIndex + 1) 
                            (Array.length model.questions) 
                            model.questions)
            }


-- View

view : Signal.Address Action -> Model -> Html
view address model =
    let context i = 
            { actions = Signal.forwardTo address (ModifyQuestion i)
            , remove = Signal.forwardTo address (always (DeleteQuestion i))
            }

        questionViews = 
            Array.indexedMap 
                (\i question -> Question.view (context i) question) 
                model.questions
            |> Array.toList
    
        addButton =
            button [onClick address NewQuestion] [text "Add"]
    in
    div 
        [ style 
            ([ ("backgroundColor", "rgb(200,255,200)")
            , ("width", "250px")
            , ("padding", "2px")
            , ("textAlign", "center")
            ] ++ HtmlUtils.bordered)
        ]
        ((HtmlUtils.title "Questions" :: questionViews) ++ [addButton])


toJson : Model -> Encode.Value
toJson model =
    Array.map Question.toJson model.questions
    |> Array.toList
    |> Encode.list


-- Events
