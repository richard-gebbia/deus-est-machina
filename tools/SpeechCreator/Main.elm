module Main where

import Array
import Char
import Conversation
import Debug
import Dict
import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Json.Encode as Encode
import JsonView
import KeyboardUtils
import List exposing ((::))
import Mouse
import Questions
import Result
import Signal exposing (Signal)
import StartApp
import Speech
import String


-- Model

type Mode
    = ViewingJson
    | ViewingGraph
    | AddingSpeech
    | AddingQuestions


type alias Model =
    { conversation: Conversation.Model
    , jsonView: JsonView.Model
    , mode: Mode
    , inputText: String
    , errorText: String
    , nextKey: Int
    , canMakeNewNodes: Bool
    }


-- Action

type Action 
    = ModifyConversation Conversation.Action
    | ModifyJsonView JsonView.Action
    | ParseConversation String
    | ViewGraph
    | ViewJson
    | IntentNewSpeech
    | IntentNewQuestions
    | EnableNodeCreation Bool
    | Click (Int, Int)
    | NoOp


-- Update

updateViewingGraph : Action -> Model -> Model
updateViewingGraph action model =
    case action of
        ModifyConversation cAction ->
            { model |
                conversation <-
                    Conversation.update cAction model.conversation
            }

        ViewJson ->
            { model | 
                mode <- ViewingJson,
                jsonView <- 
                    Conversation.toJson model.conversation
                    |> Encode.encode 4
                    |> JsonView.EditInput
                    |> flip JsonView.update model.jsonView
            }

        EnableNodeCreation isDisabled ->
            { model | canMakeNewNodes <- not isDisabled }

        IntentNewSpeech ->
            if model.canMakeNewNodes then
                { model | mode <- AddingSpeech }
            else
                model

        IntentNewQuestions ->
            if model.canMakeNewNodes then
                { model | mode <- AddingQuestions }
            else
                model

        _ ->
            model


updateViewingJson : Action -> Model -> Model
updateViewingJson action model =
    case action of
        ModifyJsonView mjwAction ->
            { model | jsonView <- JsonView.update mjwAction model.jsonView }

        ParseConversation json ->
            Decode.decodeString Conversation.fromJson json
            |> (\result -> 
                    case result of
                        Result.Ok conversation ->
                            { model | conversation <- conversation }

                        Result.Err message ->
                            { model | 
                                jsonView <- 
                                    JsonView.update 
                                        (JsonView.SetErrorText message)
                                        model.jsonView
                            }
                )

        ViewGraph ->
            { model | mode <- ViewingGraph }

        _ ->
            model


updateAddingSpeech : Action -> Model -> Model
updateAddingSpeech action model =
    let newSpeech : Int -> Int -> Conversation.Node
        newSpeech x y = 
            Conversation.Talking
                { speaker = "Ava"
                , line1 = ""
                , line2 = ""
                , line3 = ""
                , children = []
                , x = x
                , y = y
                }
    in
    case action of 
        Click (x, y) ->
            { model |
                conversation <-
                    Dict.insert 
                        (toString model.nextKey) 
                        (newSpeech x y) 
                        model.conversation,

                mode <- ViewingGraph,

                nextKey <- model.nextKey + 1
            }

        _ -> 
            model


updateAddingQuestions : Action -> Model -> Model
updateAddingQuestions action model =
    let newQuestions : Int -> Int -> Conversation.Node
        newQuestions x y = 
            Conversation.Asking
                { questions = Array.empty
                , x = x
                , y = y
                }
    in
    case action of 
        Click (x, y) ->
            { model | 
                conversation <-
                    Dict.insert 
                        (toString model.nextKey)
                        (newQuestions x y)
                        model.conversation,

                mode <- ViewingGraph,

                nextKey <- model.nextKey + 1
            }

        _ ->
            model


update : Action -> Model -> Model
update action model =
    case model.mode of
        ViewingGraph -> 
            updateViewingGraph action model

        ViewingJson -> 
            updateViewingJson action model

        AddingSpeech -> 
            updateAddingSpeech action model

        AddingQuestions ->
            updateAddingQuestions action model

        _ -> model



-- View

button : Signal.Address Action -> Action -> String -> Html
button address action text =
    Html.button [HtmlEvents.onClick address action] [Html.text text]


viewViewingGraph : Signal.Address Action -> Model -> Html
viewViewingGraph address model =
    Html.div []
        [ button address ViewJson "Json"
        , Conversation.view 
            (Conversation.Context
                (Signal.forwardTo address ModifyConversation)
                (Signal.forwardTo address EnableNodeCreation))
            model.conversation
        ]


viewViewingJson : Signal.Address Action -> Model -> Html
viewViewingJson address model =
    let context : JsonView.Context
        context =
            { actions = Signal.forwardTo address ModifyJsonView
            , submit = Signal.forwardTo address ParseConversation
            , viewGraph = Signal.forwardTo address (always ViewGraph)
            }
    in
    JsonView.view context model.jsonView


viewAddingSpeech : Signal.Address Action -> Model -> Html
viewAddingSpeech address model =
    Html.div []
        [ viewViewingGraph address model ]


viewAddingQuestions : Signal.Address Action -> Model -> Html
viewAddingQuestions address model =
    Html.div []
        [ viewViewingGraph address model ]


view : Signal.Address Action -> Model -> Html
view address model =
    case model.mode of
        ViewingGraph ->
            viewViewingGraph address model

        ViewingJson ->
            viewViewingJson address model

        AddingSpeech ->
            viewAddingSpeech address model

        AddingQuestions ->
            viewAddingQuestions address model


-- Main

init : Model
init = 
    { conversation =
        Dict.singleton "ava1" 
            (Conversation.Talking 
                { speaker = "Ava"
                , line1 = "Hello"
                , line2 = "My name is Ava"
                , line3 = ""
                , children = ["lol", "wut", "idk"]
                , x = 300
                , y = 50
                })
        |> Dict.insert "questions1"
            (Conversation.Asking
                { questions =
                    [ { line1 = "How can she slap?!"
                      , line2 = ""
                      , line3 = ""
                      , children = []
                      }
                    ] |> Array.fromList
                , x = 40
                , y = 100
                })
    , jsonView = 
        { json = ""
        , errorText = ""
        }
    , mode = ViewingGraph
    , inputText = ""
    , errorText = ""
    , nextKey = 0
    , canMakeNewNodes = True
    }


main : Signal Html
main =
    Signal.map (view appAddress) appModel


actions : Signal.Mailbox (Maybe Action)
actions =
    Signal.mailbox Nothing


appAddress : Signal.Address Action
appAddress =
    Signal.forwardTo actions.address Just


extraSignals : List (Signal Action)
extraSignals =
    [ Char.toCode 'Q' 
      |> KeyboardUtils.keyPresses 
      |> Signal.map (always IntentNewSpeech) 
    , Char.toCode 'W'
      |> KeyboardUtils.keyPresses
      |> Signal.map (always IntentNewQuestions)
    , Signal.sampleOn Mouse.clicks Mouse.position
      |> Signal.map Click
    ]


appSignals : Signal (Maybe Action)
appSignals = 
    Signal.mergeMany 
        (actions.signal :: List.map (Signal.map Just) extraSignals)


appModel : Signal Model
appModel =
    Signal.foldp
        (\(Just action) model -> update action model)
        init
        appSignals


port backgroundColor : Signal String
port backgroundColor =
    let color : Mode -> String
        color mode =
            case mode of
                AddingSpeech -> "#ffcccc"
                AddingQuestions -> "#ccccff"
                _ -> "#cccccc"
    in
    Signal.map (.mode >> color) appModel
