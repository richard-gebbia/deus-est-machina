module Speech where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlUtils
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import List
import Maybe
import Signal


-- Model

type alias Model =
    { speaker: String
    , line1: String
    , line2: String
    , line3: String
    , children: List String
    , x: Int
    , y: Int
    }
  
  
speakers : List String
speakers = 
    [ "Sophie"
    , "Gavin"
    , "Ava"
    , "Sebastian"
    ]


type alias Intermediate =
    { name: String
    , text: List String
    , children: List String
    }


fromJson : Decode.Decoder Model
fromJson =
    let toIntermediate : Decode.Decoder Intermediate
        toIntermediate = 
            Decode.object3 Intermediate
                ("name" := Decode.string)
                ("text" := Decode.list Decode.string)
                ("children" := Decode.list Decode.string)

        intermediateToModel : Intermediate -> Model
        intermediateToModel intermediate =
            { speaker = intermediate.name
            , line1 = 
                List.head intermediate.text 
                |> Maybe.withDefault ""
            , line2 = 
                List.drop 1 intermediate.text 
                |> List.head 
                |> Maybe.withDefault ""
            , line3 =
                List.drop 2 intermediate.text
                |> List.head
                |> Maybe.withDefault ""
            , children = intermediate.children
            , x = 0
            , y = 0
            }
    in
    Decode.map intermediateToModel toIntermediate


-- Action

type Action 
    = SetSpeaker String
    | SetText Int String


-- Update

update : Action -> Model -> Model
update action model =
    case action of
        SetSpeaker speaker ->
            { model | speaker <- speaker }

        SetText lineNum line ->
            { model |
                line1 <- if lineNum == 1 then line else model.line1,
                line2 <- if lineNum == 2 then line else model.line2,
                line3 <- if lineNum == 3 then line else model.line3
            }

        _ ->
            model


-- View

type alias Context = 
    { actions: Signal.Address Action
    , focus: Signal.Address Bool
    , remove: Signal.Address ()
    }


view : Context -> Model -> Html
view context model = 
    let textLine : String -> Int -> Html
        textLine txt lineNum = 
            input 
                [ style [("width", "243px")]
                , value txt 
                , on "input" targetValue (setTextMessage lineNum)
                , onFocus context.focus True
                , onBlur context.focus False
                ] []

        setTextMessage : Int -> String -> Signal.Message
        setTextMessage lineNum =
            SetText lineNum >> Signal.message context.actions

        setSpeakerMessage : String -> Signal.Message
        setSpeakerMessage =
            SetSpeaker >> Signal.message context.actions

        -- width of the div
        width : Int
        width = 250

        -- amound of vertical distance between each child link button
        childBtnStride : Int
        childBtnStride = 25

        childBtnXPadding : Int
        childBtnXPadding = 5

        childBtnYPadding : Int
        childBtnYPadding = 25

        childButton : Int -> Html
        childButton y =
            button
                [ style
                    [ ("position", "absolute")
                    , ("left", toString (width + childBtnXPadding) ++ "px")
                    , ("top", toString (childBtnYPadding + y) ++ "px")
                    ]
                ]
                [ text "+" ]

        childButtons : List Html
        childButtons =
            List.map 
                ((*) childBtnStride >> childButton) 
                [0..List.length model.children]
    in
    div 
        [ style 
            ([ ("backgroundColor", "rgb(255,255,200)")
            , ("width", toString width ++ "px")
            , ("padding", "2px")
            , ("textAlign", "center")
            , ("position", "absolute")
            , ("left", toString model.x ++ "px")
            , ("top", toString model.y ++ "px")
            ] ++ HtmlUtils.bordered)
        ]
        ([ HtmlUtils.closeButton context.remove
        , HtmlUtils.title "Speech"
        , text "Speaker "
        , HtmlUtils.selection speakers model.speaker setSpeakerMessage
        , br [] []
        , text "Text"
        , br [] []
        , textLine model.line1 1
        , br [] []
        , textLine model.line2 2
        , br [] []
        , textLine model.line3 3
        ] ++ childButtons)
    

debugDraw : Model -> Html
debugDraw model =
    div
        []
        [ text ("Speaker: " ++ model.speaker)
        , br [] []
        , text ("Line 1: " ++ model.line1)
        , br [] []
        , text ("Line 2: " ++ model.line2)
        , br [] []
        , text ("Line 3: " ++ model.line3)
        ]


toJson : Model -> Encode.Value
toJson model =
    let text : List Encode.Value
        text = List.map Encode.string [model.line1, model.line2, model.line3]
    in
    Encode.object
        [ ("name", Encode.string model.speaker) 
        , ("text", Encode.list text)
        , ("children", Encode.list (List.map Encode.string model.children))
        ]


-- Events

