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
    , x: Int
    , y: Int
    }


init : Int -> Int -> Model
init x y =
    { questions = Array.empty
    , x = x
    , y = y
    }


fromJson : Decode.Decoder Model
fromJson =
    Decode.list Question.fromJson
    |> Decode.map Array.fromList
    |> Decode.map (\questions -> Model questions 0 0)


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

type alias Context =
    { actions: Signal.Address Action
    , focus: Signal.Address Bool
    , remove: Signal.Address ()
    }


view : Context -> Model -> Html
view context model =
    let subContext i = 
            { actions = Signal.forwardTo context.actions (ModifyQuestion i)
            , remove = Signal.forwardTo context.actions (always (DeleteQuestion i))
            , focus = Signal.forwardTo context.focus identity
            }

        questionViews = 
            Array.indexedMap 
                (\i question -> Question.view (subContext i) question) 
                model.questions
            |> Array.toList
    
        addButton =
            button [onClick context.actions NewQuestion] [text "Add"]
    in
    div 
        [ style 
            ([ ("backgroundColor", "rgb(200,255,200)")
            , ("width", "250px")
            , ("padding", "2px")
            , ("position", "absolute")
            , ("left", toString model.x ++ "px")
            , ("top", toString model.y ++ "px")
            , ("textAlign", "center")
            ] ++ HtmlUtils.bordered)
        ]
        ((  HtmlUtils.closeButton context.remove 
        ::  HtmlUtils.title "Questions" 
        ::  questionViews) 
        ++  [addButton])


toJson : Model -> Encode.Value
toJson model =
    Array.map Question.toJson model.questions
    |> Array.toList
    |> Encode.list


-- Events
