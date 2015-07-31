module HtmlUtils where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import List exposing ((::))


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
    

selection : List String -> String -> (String -> Signal.Message) -> Html
selection list selectedStr onChange = 
    let attrs : String -> List Attribute
        attrs str = 
            value str :: if str == selectedStr then [selected True] else [] 
    in
    select [on "change" targetValue onChange]
        <| List.map (\str -> option (attrs str) [ text str ]) list
