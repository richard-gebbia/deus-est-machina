module Speech where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlUtils
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import List
import Maybe
import Parentable
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


init : Int -> Int -> Model
init x y =
    { speaker = "Sophie"
    , line1 = ""
    , line2 = ""
    , line3 = ""
    , children = []
    , x = x
    , y = y 
    }


type alias Intermediate =
    { name: String
    , text: List String
    , children: List String
    }


save : Model -> Encode.Value
save model =
    Encode.object
        [ ("speaker", Encode.string model.speaker)
        , ("line1", Encode.string model.line1)
        , ("line2", Encode.string model.line2)
        , ("line3", Encode.string model.line3)
        , ("children", Encode.list (List.map Encode.string model.children))
        , ("x", Encode.int model.x)
        , ("y", Encode.int model.y)
        ]


load : Decode.Decoder Model
load = 
    Decode.object7 Model
        ("speaker" := Decode.string)
        ("line1" := Decode.string)
        ("line2" := Decode.string)
        ("line3" := Decode.string)
        ("children" := Decode.list Decode.string)
        ("x" := Decode.int)
        ("y" := Decode.int)


-- Action

type Action 
    = SetSpeaker String
    | SetText Int String
    | ModifyParentable Parentable.Action


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

        ModifyParentable pAction ->
            Parentable.update pAction model

        _ ->
            model


-- View

 -- width of the div
width : Int
width = 250

-- amound of vertical distance between each child link button
childBtnStride : Int
childBtnStride = 18


childBtnXPadding : Int
childBtnXPadding = 5


childBtnYPadding : Int
childBtnYPadding = 25


childBtnPosition : Model -> Int -> (Int, Int)
childBtnPosition model index =
    (
        width + childBtnXPadding + model.x, 
        (index * childBtnStride + childBtnYPadding) + model.y
    )


view : Events -> Model -> Html
view events model = 
    let textLine : String -> Int -> Html
        textLine txt lineNum = 
            input 
                [ style [("width", "243px")]
                , value txt 
                , on "input" targetValue (setTextMessage lineNum)
                , onFocus events.focus True
                , onBlur events.focus False
                ] []

        setTextMessage : Int -> String -> Signal.Message
        setTextMessage lineNum =
            SetText lineNum >> Signal.message events.actions

        setSpeakerMessage : String -> Signal.Message
        setSpeakerMessage =
            SetSpeaker >> Signal.message events.actions

        backgroundColor : String
        backgroundColor =
            case model.speaker of
                "Sophie" -> "rgb(200,200,255)"
                "Gavin" -> "rgb(255,200,200)"
                "Ava" -> "rgb(200,255,200)"
                "Sebastian" -> "rgb(255,255,200)"
    in
    div [ style
            [ ("position", "absolute")
            , ("left", toString model.x ++ "px")
            , ("top", toString model.y ++ "px")
            , ("width", toString (6 * width) ++ "px")
            , ("height", "600px")
            ]
        ]
        [ div 
            [ style 
                ([ ("backgroundColor", backgroundColor)
                , ("width", toString width ++ "px")
                , ("padding", "2px")
                , ("textAlign", "center")
                ] ++ HtmlUtils.bordered)
            ]
            [ button 
                [ style [ ("float", "left" ) ]
                , onClick events.parentThis () 
                ] [ text "child"]
            , HtmlUtils.closeButton events.remove
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
            , Parentable.view 
                width
                childBtnXPadding
                childBtnYPadding
                childBtnStride
                (Parentable.Events
                    (Signal.forwardTo events.actions ModifyParentable)
                    (Signal.forwardTo events.startParenting (always ())))
                model
            ]
        ]
        

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

type alias Events = 
    { actions: Signal.Address Action
    , focus: Signal.Address Bool
    , remove: Signal.Address ()
    , startParenting: Signal.Address ()
    , parentThis: Signal.Address ()
    }