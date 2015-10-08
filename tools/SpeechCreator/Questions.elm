module Questions where

import Array exposing (Array)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlUtils
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Maybe
import Parentable
import Question

-- Model 

type alias Model =
    { questions: Array Question.Model
    , x: Int
    , y: Int
    }


yStart : Int
yStart = 37


yStride : Int
yStride = 102


init : Int -> Int -> Model
init x y =
    { questions = Array.empty
    , x = x
    , y = y
    }


toParentSpace : Model -> (Int, Int) -> (Int, Int)
toParentSpace model (x, y) =
    (x + model.x, y + model.y)


save : Model -> Encode.Value
save model =
    Encode.object
        [ ("questions", Encode.array (Array.map Question.save model.questions))
        , ("x", Encode.int model.x)
        , ("y", Encode.int model.y)
        ]


load : Decode.Decoder Model
load =
    Decode.object3 Model
        ("questions" := Decode.array Question.load)
        ("x" := Decode.int)
        ("y" := Decode.int)


-- Action

type Action
    = ModifyQuestion Int Question.Action
    | NewQuestion
    | DeleteQuestion Int
    | ParentQuestion Int String


-- Update

update : Action -> Model -> Model
update action model =
    let questionY : Int -> Int
        questionY index =
            index * yStride + yStart
    in
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
                    Array.push 
                        (Question.init 0 
                            (questionY (Array.length model.questions))) 
                        model.questions
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
                    |> Array.indexedMap 
                        (\index question -> 
                            { question | y <- questionY index })
            }

        ParentQuestion questionIndex childKey ->
            update 
                (ModifyQuestion questionIndex 
                    (Question.ModifyParentable 
                        (Parentable.AddChild childKey)))
                model


-- View

view : Events -> Model -> Html
view events model =
    let qEvents : Int -> Question.Events
        qEvents i = 
            { actions = Signal.forwardTo events.actions (ModifyQuestion i)
            , remove = Signal.forwardTo events.actions (always (DeleteQuestion i))
            , focus = Signal.forwardTo events.focus identity
            , startParenting = Signal.forwardTo events.startParenting (always i)
            }

        questionViews : List Html
        questionViews = 
            Array.indexedMap 
                (\i question -> Question.view (qEvents i) question) 
                model.questions
            |> Array.toList
    
        addButton : Html
        addButton =
            button [onClick events.actions NewQuestion] [text "Add"]

        questionGap : Html
        questionGap =
            div
                [ style
                    [ ("width", "250px")
                    , ("height", 
                        toString (yStride * 
                            Array.length model.questions) ++ "px")
                    ]
                ] []
    in
    div 
        [ style
            [ ("position", "absolute")
            , ("left", toString model.x ++ "px")
            , ("top", toString model.y ++ "px")
            , ("width", "750px")
            , ("height", toString (Array.length model.questions * yStride + 600) ++ "px" )
            ]
        ]
        [ div 
            [ style 
                ([ ("backgroundColor", "rgb(150,150,150)")
                , ("padding", "2px")
                , ("textAlign", "center")
                , ("width", "250px")
                ] ++ HtmlUtils.bordered)
            ]
            (  button 
                [ style [ ("float", "left") ]
                , onClick events.parentThis () 
                ] [ text "child" ]
            :: HtmlUtils.closeButton events.remove 
            :: HtmlUtils.title "Questions" 
            :: questionGap
            :: addButton
            :: questionViews
            ) ]


toJson : Model -> Encode.Value
toJson model =
    Array.map Question.toJson model.questions
    |> Array.toList
    |> Encode.list


-- Events

type alias Events =
    { actions: Signal.Address Action
    , focus: Signal.Address Bool
    , remove: Signal.Address ()
    , startParenting: Signal.Address Int
    , parentThis: Signal.Address ()
    }

