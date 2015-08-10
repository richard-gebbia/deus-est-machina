module Main where

import Array
import Char
import Conversation
import Debug
import Dict
import GraphView
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
import Speech
import String


-- Model

type Mode
    = ViewingJson
    | ViewingGraph


type alias Model =
    { graphView: GraphView.Model
    , jsonView: JsonView.Model
    , mode: Mode
    }


-- Action

type Action 
    = ModifyGraphView GraphView.Action
    | ModifyJsonView JsonView.Action
    | ParseConversation String
    | ViewGraph
    | ViewJson
    | IntentToggleViews
    | Click (Int, Int)
    | NoOp


-- Update

updateViewingGraph : Action -> Model -> Model
updateViewingGraph action model =
    case action of
        ModifyGraphView gvAction ->
            { model |
                graphView <-
                    GraphView.update gvAction model.graphView
            }

        ViewJson ->
            { model | 
                mode <- ViewingJson,
                jsonView <- 
                    Conversation.toJson model.graphView.conversation
                    |> Encode.encode 4
                    |> JsonView.EditInput
                    |> flip JsonView.update model.jsonView
            }

        IntentToggleViews ->
            if model.graphView.focus then
                updateViewingGraph ViewJson model
            else
                model

        Click (x, y) ->
            { model |
                graphView <-
                    GraphView.update (GraphView.Click (x, y)) model.graphView
            }

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
                            { model | 
                                graphView <-
                                    GraphView.update 
                                        (GraphView.SetConversation conversation)
                                        model.graphView
                            }

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

        IntentToggleViews ->
            if model.jsonView.focus then
                { model | mode <- ViewingGraph }
            else 
                model

        _ ->
            model


update : Action -> Model -> Model
update action model =
    case model.mode of
        ViewingGraph -> 
            updateViewingGraph action model

        ViewingJson -> 
            updateViewingJson action model

        _ -> 
            model



-- View

button : Signal.Address Action -> Action -> String -> Html
button address action text =
    Html.button [HtmlEvents.onClick address action] [Html.text text]


viewViewingGraph : Signal.Address Action -> Model -> Html
viewViewingGraph address model =
    Html.div []
        [ button address ViewJson "Json"
        , GraphView.view 
            (Signal.forwardTo address ModifyGraphView)
            model.graphView
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


view : Signal.Address Action -> Model -> Html
view address model =
    case model.mode of
        ViewingGraph ->
            viewViewingGraph address model

        ViewingJson ->
            viewViewingJson address model


-- Main

init : Model
init = 
    { graphView =
        { conversation = 
            Dict.singleton "ava1" 
                (Conversation.Talking 
                    { speaker = "Ava"
                    , line1 = "Hello"
                    , line2 = "My name is Ava"
                    , line3 = ""
                    , children = ["lol", "wut", "idk"]
                    , x = 500
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
        , mode = GraphView.ViewingGraph
        , nextKey = 0
        , focus = True
        }
    , jsonView = 
        { json = ""
        , errorText = ""
        , focus = True
        }
    , mode = ViewingGraph
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
      |> Signal.map (always (ModifyGraphView GraphView.IntentAddSpeech))
    , Char.toCode 'W'
      |> KeyboardUtils.keyPresses
      |> Signal.map (always (ModifyGraphView GraphView.IntentAddQuestions))
    , Char.toCode '\t'
      |> KeyboardUtils.keyPresses
      |> Signal.map (always IntentToggleViews)
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
    let color : GraphView.Mode -> String
        color mode =
            case mode of
                GraphView.AddingSpeech -> "rgb(255,255,200)"
                GraphView.AddingQuestions -> "rgb(200,255,200)"
                _ -> "#cccccc"
    in
    Signal.map (.graphView >> .mode >> color) appModel
