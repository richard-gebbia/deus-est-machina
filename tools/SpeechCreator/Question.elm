module Question where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlUtils
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Signal

-- Model

type alias Model = 
    { line1: String
    , line2: String
    , line3: String
    , children: List String
    }


init : Model
init =
    { line1 = ""
    , line2 = ""
    , line3 = ""
    , children = [] 
    }


type alias Intermediate =
    { text: List String
    , children: List String
    }


fromJson : Decode.Decoder Model
fromJson =
    let toIntermediate : Decode.Decoder Intermediate
        toIntermediate = 
            Decode.object2 Intermediate
                ("text" := Decode.list Decode.string)
                ("children" := Decode.list Decode.string)

        intermediateToModel : Intermediate -> Model
        intermediateToModel intermediate =
            { line1 = 
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
            }
    in
    Decode.map intermediateToModel toIntermediate


-- Action

type Action
    = SetText Int String


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

        _ ->
            model


-- View

type alias Context =
    { actions : Signal.Address Action
    , remove : Signal.Address ()
    }


view : Context -> Model -> Html
view context model =
    let textLine : String -> Int -> Html
        textLine txt lineNum = 
            input 
                [ style [("width", "243px")]
                , value txt 
                , on "input" targetValue (setTextMessage lineNum)
                ] []

        setTextMessage : Int -> String -> Signal.Message
        setTextMessage lineNum =
            SetText lineNum >> Signal.message context.actions
    in
    div 
        [ style 
            ([ ("backgroundColor", "rgb(200,200,255)")
            , ("width", "250px")
            , ("padding", "2px")
            , ("textAlign", "center")
            ] ++ HtmlUtils.bordered)
        ]
        [ text "Question"
        , br [] []
        , textLine model.line1 1
        , br [] []
        , textLine model.line2 2
        , br [] []
        , textLine model.line3 3
        , br [] []
        , button [onClick context.remove ()] [text "Delete"]
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
