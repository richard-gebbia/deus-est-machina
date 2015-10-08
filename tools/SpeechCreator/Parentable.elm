module Parentable where

import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import List

-- Model

type alias Model a =
    { a | children: List String }


-- Action

type Action 
    = AddChild String
    | RemoveChild Int


-- Update

update : Action -> Model a -> Model a
update action model =
    case action of
        AddChild child ->
            { model | 
                children <-
                    model.children ++ [child]
            }

        RemoveChild index ->
            { model |
                children <-
                    List.take index       model.children ++
                    List.drop (index + 1) model.children
            }


-- View

view : Int -> Int -> Int -> Int -> Events -> Model a -> Html
view width xPadding yPadding yStride events model =
    let removeChildButton : Int -> Html
        removeChildButton index =
            Html.button
                [ HtmlAttrs.style
                    [ ("position", "absolute")
                    , ("top", toString (index * yStride) ++ "px")
                    ]
                , HtmlEvents.onClick events.actions (RemoveChild index)
                ]
                [ Html.text "-" ]

        addChildButton : Int -> Html
        addChildButton index =
            Html.button
                [ HtmlAttrs.style
                    [ ("position", "absolute")
                    , ("top", toString (index * yStride) ++ "px")
                    ]
                , HtmlEvents.onClick events.startParenting ()
                ]
                [ Html.text "+" ]

        childButtons : List Html
        childButtons =
            (List.map removeChildButton 
                [0..(List.length model.children - 1)])
            ++ [addChildButton (List.length model.children)]
    in
    Html.div 
        [ HtmlAttrs.style 
            [ ("position", "absolute")
            , ("left", toString (width + xPadding) ++ "px")
            , ("top", toString yPadding ++ "px")
            ]
        ] 
        childButtons


-- Events

type alias Events =
    { actions: Signal.Address Action
    , startParenting: Signal.Address ()
    }
