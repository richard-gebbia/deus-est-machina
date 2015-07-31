module Main where

import Array
import Conversation
import Dict
import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing ((::))
import Questions
import Signal exposing (Signal)
import StartApp
import Speech
import String


-- Model

type ViewMode
    = ViewingJson
    | ViewingGraph


type alias Model =
    { conversation: Conversation.Model
    , viewMode: ViewMode
    , inputText: String
    , errorText: String
    }


-- Action

type Action 
    = ModifyConversation Conversation.Action
    | EditInput String
    | SubmitInput
    | ViewJson
    | ViewGraph


-- Update

update : Action -> Model -> Model
update action model =
    case action of
        ModifyConversation cAction ->
            { model | 
                conversation <- 
                    Conversation.update cAction model.conversation
            }

        EditInput str ->
            { model | inputText <- str }

        SubmitInput ->
            Decode.decodeString Conversation.fromJson model.inputText
            |> (\result -> 
                    case result of
                        Ok conversation -> 
                            { model | conversation <- conversation }

                        Err errorString ->
                            { model | errorText <- errorString }
                )

        ViewJson ->
            { model | viewMode <- ViewingJson }

        ViewGraph ->
            { model | viewMode <- ViewingGraph }

        _ -> 
            model


-- View

view : Signal.Address Action -> Model -> Html
view address model =
    let onClick : Action -> Html.Attribute
        onClick action =
            HtmlEvents.onClick address action

        textareaWithText : String -> Html
        textareaWithText str =
            Html.textarea
                [ HtmlAttrs.value str
                , HtmlAttrs.style 
                    [ ("width", "400px")
                    , ("height", "600px")
                    ]
                , EditInput >> Signal.message address 
                  |> HtmlEvents.on "input" HtmlEvents.targetValue
                ]
                []
    in
    case model.viewMode of 
        ViewingJson -> 
            Html.div []
                [ Html.button [onClick ViewGraph] [Html.text "Graph"]
                , Html.br [] []
                , Conversation.toJson model.conversation
                  |> Encode.encode 4 
                  |> textareaWithText
                , Html.br [] []
                , Html.button [onClick SubmitInput] [Html.text "Submit"]
                , Html.br [] []
                , Html.div 
                    [HtmlAttrs.style [("color", "red")]] 
                    [Html.text model.errorText]
                ]

        ViewingGraph ->
            Html.div []
                [ Html.button [onClick ViewJson] [Html.text "Encode"]
                , Conversation.view 
                    (Signal.forwardTo address ModifyConversation)
                    model.conversation
                ]



-- Events

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
                , children = []
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
                })
    , viewMode = ViewingGraph
    , inputText = ""
    , errorText = ""
    }


main : Signal Html
main =
    StartApp.start { model = init, update = update, view = view }