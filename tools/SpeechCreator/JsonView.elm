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
    }


-- Action

type Action 
    = SupplyJsonAndSaveData String String
    | EditLoadData String
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

        SetFocus focus ->
            { model | focus <- focus }

        _ ->
            model

-- View

type alias Context =
    { actions: Signal.Address Action 
    , viewGraph: Signal.Address ()
    }


view : Context -> Model -> Html
view context model =
    let width : Int
        width = 400

        height : Int
        height = 600

        jsonTextArea : String -> String -> List Html.Attribute -> Html
        jsonTextArea title content attrs =
            Html.div [ HtmlAttrs.style [("display", "inline-block")] ]
                [ Html.h3 [] [Html.text title]
                , Html.textarea
                    ([ HtmlAttrs.value content
                    , HtmlAttrs.style 
                        [ ("width", toString width ++ "px")
                        , ("height", toString height ++ "px")
                        ]
                    , HtmlEvents.onFocus context.actions (SetFocus False)
                    , HtmlEvents.onFocus context.actions (SetFocus True)
                    ] ++ attrs) []
                ]
    in
    Html.div []  
        [ HtmlUtils.button context.viewGraph () "Graph"
        , Html.div []
            [ jsonTextArea "Conversation" model.json []
            , jsonTextArea "Save" model.saveData []
            , jsonTextArea "Load" model.loadData 
                [ EditLoadData >> Signal.message context.actions |>
                  HtmlEvents.on "input" HtmlEvents.targetValue ]
            ]
        ]
