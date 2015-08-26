module JsonView where

import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import HtmlUtils
import Signal

-- Model

type alias Model =
    { json: String
    , saveData: String
    , loadData: String
    , focus: Bool
    , errorText: String
    }


-- Action

type Action 
    = SupplyJsonAndSaveData String String
    | EditLoadData String
    | SetErrorText String
    | SetFocus Bool


-- Update

update : Action -> Model -> Model
update action model =
    case action of
        SupplyJsonAndSaveData json saveData ->
            { model | 
                json <- json,
                saveData <- saveData,
                loadData <- ""
            }

        EditLoadData loadData ->
            { model | loadData <- loadData }

        SetErrorText errorText ->
            { model | errorText <- errorText }

        SetFocus focus ->
            { model | focus <- focus }

        _ ->
            model

-- View

type alias Context =
    { actions: Signal.Address Action 
    , submit: Signal.Address String
    , viewGraph: Signal.Address ()
    }


view : Context -> Model -> Html
view context model =
    let width : Int
        width = 400

        height : Int
        height = 500

        jsonTextArea : String -> String -> String -> Html
        jsonTextArea title desc content =
            Html.div [ HtmlAttrs.style [("display", "inline-block")] ]
                [ Html.h3 [] [Html.text title]
                , Html.div [] [Html.text desc]
                , Html.textarea
                    [ HtmlAttrs.value content
                    , HtmlAttrs.style 
                        [ ("width", toString width ++ "px")
                        , ("height", toString height ++ "px")
                        ]
                    , HtmlEvents.onFocus context.actions (SetFocus False)
                    , HtmlEvents.onFocus context.actions (SetFocus True)
                    ] []
                ]
    in
    Html.div []  
        [ HtmlUtils.button context.viewGraph () "Graph"
        , Html.div []
            [ jsonTextArea "Conversation" "Replace conversation.js with this" model.json
            , jsonTextArea "Save" "Copy this into a separate file" model.saveData
            , Html.div [ HtmlAttrs.style [("display", "inline-block")] ]
                [ Html.h3 [] [Html.text "Load"]
                , Html.div [] 
                    [ Html.div []
                        [ Html.text "Put what you \"save\"d here and press " 
                        , HtmlUtils.button context.submit model.loadData "Submit"
                        ]
                    ]
                , Html.textarea
                    [ HtmlAttrs.value model.loadData
                    , HtmlAttrs.style
                        [ ("width", toString width ++ "px")
                        , ("height", toString height ++ "px")
                        ]
                    , HtmlEvents.onFocus context.actions (SetFocus False)
                    , HtmlEvents.onBlur context.actions (SetFocus True)
                    , EditLoadData >> Signal.message context.actions |>
                        HtmlEvents.on "input" HtmlEvents.targetValue
                    ] []
                , Html.div 
                    [ HtmlAttrs.style [("color", "red")] ]
                    [ Html.text model.errorText ]
                ]
            ]
        ]
