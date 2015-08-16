module HtmlUtils where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import List exposing ((::))
import Signal


bordered : List (String, String)
bordered = 
    [ ("border", "solid")
    , ("borderColor", "black")
    , ("borderWidth", "1px")
    ]
  
  
title : String -> Html
title str = 
    div
        [ style [("textAlign", "center")] ]
        [ b [] [text str] ]


closeButton : Signal.Address () -> Html
closeButton address =
    div 
        [ style [("float", "right")] ]
        [ button address () "x" ]


selection : List String -> String -> (String -> Signal.Message) -> Html
selection list selectedStr onChange = 
    let attrs : String -> List Attribute
        attrs str = 
            value str :: if str == selectedStr then [selected True] else [] 
    in
    select [on "change" targetValue onChange]
        <| List.map (\str -> option (attrs str) [ text str ]) list


button : Signal.Address a -> a -> String -> Html
button address action btnText =
    Html.button [onClick address action] [text btnText]


textarea : Int -> Int -> Signal.Address a -> (String -> a) -> String -> Html
textarea width height address onInput str =
    Html.textarea
        [ value str
        , style 
            [ ("width", toString width ++ "px")
            , ("height", toString height ++ "px")
            ]
        , onInput >> Signal.message address 
          |> on "input" targetValue
        ]
        []
