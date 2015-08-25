module Question where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlUtils
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Parentable
import Signal

-- Model

type alias Model = 
    { line1: String
    , line2: String
    , line3: String
    , children: List String
    , x: Int
    , y: Int
    }


init : Int -> Int -> Model
init x y =
    { line1 = ""
    , line2 = ""
    , line3 = ""
    , children = [] 
    , x = x
    , y = y
    }


type alias Intermediate =
    { text: List String
    , children: List String
    }


save : Model -> Encode.Value
save model =
    Encode.object
        [ ("line1", Encode.string model.line1)
        , ("line2", Encode.string model.line2)
        , ("line3", Encode.string model.line3)
        , ("children", Encode.list (List.map Encode.string model.children))
        , ("x", Encode.int model.x)
        , ("y", Encode.int model.y)
        ]


load : Decode.Decoder Model
load = 
    Decode.object6 Model
        ("line1" := Decode.string)
        ("line2" := Decode.string)
        ("line3" := Decode.string)
        ("children" := Decode.list Decode.string)
        ("x" := Decode.int)
        ("y" := Decode.int)


width : Int
width = 250


childBtnXPadding : Int
childBtnXPadding = 5


childBtnYPadding : Int
childBtnYPadding = 8


childBtnStride : Int
childBtnStride = 18


childBtnPosition : Model -> Int -> (Int, Int)
childBtnPosition model index =
    (
        width + childBtnXPadding + model.x, 
        (index * childBtnStride + childBtnYPadding) + model.y
    )


-- Action

type Action
    = SetText Int String
    | ModifyParentable Parentable.Action


-- Update

update : Action -> Model -> Model
update action model =
    case action of 
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

type alias Context =
    { actions : Signal.Address Action
    , remove : Signal.Address ()
    , focus : Signal.Address Bool
    , startParenting: Signal.Address ()
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
    in
    div 
        [ style 
            ([ ("backgroundColor", "rgb(200,200,255)")
            , ("width", toString width ++ "px")
            , ("padding", "2px")
            , ("textAlign", "center")
            , ("position", "absolute")
            , ("left", toString model.x ++ "px")
            , ("top", toString model.y ++ "px")
            ] ++ HtmlUtils.bordered)
        ]
        [ HtmlUtils.closeButton context.remove
        , text "Question"
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
            (Parentable.Context
                (Signal.forwardTo context.actions ModifyParentable)
                (Signal.forwardTo context.startParenting (always ())))
            model
        ]


toJson : Model -> Encode.Value
toJson model =
    let text : List Encode.Value
        text = List.map Encode.string [model.line1, model.line2, model.line3]
    in
    Encode.object
        [ ("text", Encode.list text)
        , ("children", Encode.list (List.map Encode.string model.children))
        ]


-- Events
