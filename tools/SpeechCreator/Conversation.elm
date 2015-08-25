module Conversation where

import Array exposing (Array)
import ArrayUtils
import Debug
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonView
import Maybe
import Question
import Questions
import Set exposing (Set)
import Signal
import Speech


type Node 
    = Talking Speech.Model
    | Asking Questions.Model


type alias Model = Dict String Node


genericToJson : (String -> Node -> Encode.Value) -> Model -> Encode.Value
genericToJson nodeToValue model =
    Dict.map nodeToValue model
    |> Dict.toList
    |> Encode.object


toJson : Model -> Encode.Value
toJson model =
    let nodeToJson : String -> Node -> Encode.Value
        nodeToJson _ node =
            case node of
                Talking speech -> 
                    Speech.toJson speech

                Asking questions ->
                    Questions.toJson questions
    in
    genericToJson nodeToJson model


save : Model -> Encode.Value
save model =
    let nodeToJson : String -> Node -> Encode.Value
        nodeToJson _ node =
            case node of
                Talking speech -> 
                    Speech.save speech

                Asking questions ->
                    Questions.save questions
    in
    genericToJson nodeToJson model


removeNode : String -> Model -> Model
removeNode key model =
    let removeFromQuestion : String -> Question.Model -> Question.Model
        removeFromQuestion key question =
            { question |
                children <-
                    List.filter ((/=) key) question.children
            }

        removeFromChildren : String -> String -> Node -> Node
        removeFromChildren key _ node =
            case node of
                Talking speech ->
                    { speech | 
                        children <-
                            List.filter ((/=) key) speech.children
                    } |> Talking

                Asking questions ->
                    { questions |
                        questions <- 
                            Array.map 
                                (removeFromQuestion key)
                                questions.questions
                    } |> Asking
    in
    Dict.remove key model
    |> Dict.map (removeFromChildren key)

