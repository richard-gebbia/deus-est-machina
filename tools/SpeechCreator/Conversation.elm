module Conversation where

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe
import Questions
import Signal
import Speech

-- Model

type Node 
    = Talking Speech.Model
    | Asking Questions.Model


type alias Model = Dict String Node


fromJson : Decode.Decoder Model
fromJson =
    Decode.oneOf
        [ Speech.fromJson |> Decode.map Talking
        , Questions.fromJson |> Decode.map Asking 
        ]
    |> Decode.dict


-- Action

type Action 
    = ModifySpeech String Speech.Action
    | ModifyQuestions String Questions.Action


-- Update

update : Action -> Model -> Model
update action model =
    let modifySpeech : Speech.Action -> Node -> Node
        modifySpeech sAction node =
            case node of
                Talking speech ->
                    Speech.update sAction speech
                    |> Talking

                _ ->
                    node

        modifyQuestions : Questions.Action -> Node -> Node
        modifyQuestions qAction node =
            case node of
                Asking questions ->
                    Questions.update qAction questions
                    |> Asking

                _ ->
                    node
    in
    case action of
        ModifySpeech key sAction ->
            Dict.update key (Maybe.map (modifySpeech sAction)) model

        ModifyQuestions key qAction ->
            Dict.update key (Maybe.map (modifyQuestions qAction)) model

        _ ->
            model


-- View

viewNode : Signal.Address Action -> String -> Node -> Html
viewNode address key node =
    case node of 
        Talking speech ->
            Speech.view 
                (Signal.forwardTo address (ModifySpeech key)) 
                speech

        Asking questions ->
            Questions.view 
                (Signal.forwardTo address (ModifyQuestions key)) 
                questions


view : Signal.Address Action -> Model -> Html
view address model =
    Dict.map (viewNode address) model
    |> Dict.values
    |> Html.div []


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
    Dict.map nodeToJson model
    |> Dict.toList
    |> Encode.object


-- Events
