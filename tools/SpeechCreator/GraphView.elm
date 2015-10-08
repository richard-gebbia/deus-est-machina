module GraphView where

import Array
import Conversation
import Dict
import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import List exposing ((::))
import Maybe exposing (andThen)
import Parentable
import Question
import Questions
import Speech


-- Model

type Mode 
    = ViewingGraph
    | AddingSpeech
    | AddingQuestions


type Parent
    = PSpeech String
    | PQuestion String Int


type alias Model = 
    { conversation: Conversation.Model
    , mode: Mode
    , focus: Bool
    , currentlyParenting: Maybe Parent
    }


-- Action

type Action
    = ModifySpeech String Speech.Action
    | ModifyQuestions String Questions.Action
    | SetConversation Conversation.Model
    | Click (Int, Int)
    | SetFocus Bool
    | IntentAddSpeech
    | IntentAddQuestions
    | RemoveNode String
    | StartParenting Parent
    | SetChild String


-- Update

updateViewingGraph : Action -> Model -> Model
updateViewingGraph action model =
    let updateSpeech : Speech.Action -> Conversation.Node -> Conversation.Node
        updateSpeech sAction node =
            case node of
                Conversation.Talking speech ->
                    Speech.update sAction speech |> Conversation.Talking

                _ -> 
                    node

        updateQuestions : Questions.Action -> Conversation.Node -> Conversation.Node
        updateQuestions qAction node =
            case node of 
                Conversation.Asking questions ->
                    Questions.update qAction questions |> Conversation.Asking

                _ ->
                    node

        parentSpeech : String -> Conversation.Node -> Conversation.Node
        parentSpeech childKey node =
            case node of
                Conversation.Talking speech -> 
                    Parentable.update (Parentable.AddChild childKey) speech
                    |> Conversation.Talking

                _ -> 
                    node

        parentQuestion : String -> Int -> Conversation.Node -> Conversation.Node
        parentQuestion childKey qIndex node =
            case node of
                Conversation.Asking questions ->
                    Questions.update 
                        (Questions.ParentQuestion qIndex childKey)
                        questions
                    |> Conversation.Asking

                _ ->
                    node
    in
    case action of
        ModifySpeech key sAction ->
            { model | 
                conversation <-
                    Dict.update 
                        key 
                        (Maybe.map (updateSpeech sAction)) 
                        model.conversation
            }

        ModifyQuestions key qAction ->
            { model |
                conversation <- 
                    Dict.update 
                        key 
                        (Maybe.map (updateQuestions qAction)) 
                        model.conversation
            }

        SetConversation conversation ->
            { model | conversation <- conversation }

        SetFocus focus ->
            { model | focus <- focus }

        IntentAddSpeech ->
            { model | 
                mode <- if model.focus then AddingSpeech else model.mode
            }

        IntentAddQuestions ->
            { model |
                mode <- if model.focus then AddingQuestions else model.mode
            }

        RemoveNode key ->
            { model |
                conversation <- Conversation.removeNode key model.conversation
            }

        StartParenting parent ->
            { model | currentlyParenting <- Just parent }

        SetChild key ->
            case model.currentlyParenting of
                Just (PSpeech speechKey) ->
                    { model |
                        conversation <-
                            Dict.update speechKey
                                (Maybe.map (parentSpeech key))
                                model.conversation,

                        currentlyParenting <- Nothing
                    }

                Just (PQuestion qKey qIndex) ->
                    { model |
                        conversation <-
                            Dict.update qKey
                                (Maybe.map (parentQuestion key qIndex))
                                model.conversation,

                        currentlyParenting <- Nothing
                    }

                _ ->
                    model

        _ ->
            model


nextPossible : (a -> Bool) -> (a -> a) -> a -> a
nextPossible pred generator val =
    if pred val then 
        val 
    else 
        nextPossible pred generator (generator val)


addNode : Conversation.Node -> Conversation.Model -> Conversation.Model
addNode node conversation =
    Dict.insert 
        (toString (nextKey conversation))
        node
        conversation


nextKey : Conversation.Model -> Int
nextKey conversation =
    nextPossible 
        (\val -> 
            not <| Dict.member (toString val) conversation)
        ((+) 1)
        0


updateAddingSpeech : Action -> Model -> Model
updateAddingSpeech action model =
    case action of
        Click (x, y) ->
            { model |
                conversation <- 
                    addNode 
                        (Conversation.Talking <| Speech.init x y) 
                        model.conversation,

                mode <-
                    ViewingGraph
            }

        _ ->
            model


updateAddingQuestions : Action -> Model -> Model
updateAddingQuestions action model =
    case action of 
        Click (x, y) ->
            { model |
                conversation <-
                    addNode 
                        (Conversation.Asking <| Questions.init x y) 
                        model.conversation,

                mode <-
                    ViewingGraph
            }

        _ ->
            model


update : Action -> Model -> Model
update action model =
    case model.mode of
        ViewingGraph ->
            updateViewingGraph action model

        AddingSpeech ->
            updateAddingSpeech action model

        AddingQuestions ->
            updateAddingQuestions action model


-- View

viewConnection : Model -> String -> (Int, Int) -> List Html
viewConnection model child (x, y) =
    let childPos : Conversation.Node -> (Int, Int)
        childPos node = 
            case node of
                Conversation.Talking speech -> (speech.x - 5, speech.y)             -- why this is off by 5 pixels? no one knows
                Conversation.Asking questions -> (questions.x, questions.y)

        angle : (Int, Int) -> (Int, Int) -> Float
        angle (x0, y0) (x1, y1) =
            atan2 (toFloat (y1 - y0)) (toFloat (x1 - x0)) 
            |> (\ang -> ang * 180 / pi)

        dist : (Int, Int) -> (Int, Int) -> Int
        dist (x0, y0) (x1, y1) =
            (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0) 
            |> toFloat
            |> sqrt 
            |> round

        drawLine : String -> Int -> (Int, Int) -> (Int, Int) -> Html
        drawLine color thickness (x0, y0) (x1, y1) =
            let rotate : String
                rotate = "rotate(" ++ toString (angle (x0, y0) (x1, y1)) ++ "deg)"

                width : Int
                width = dist (x0, y0) (x1, y1)

                moveX : String
                moveX = toString ((x0 + x1) // 2 - width // 2) ++ "px"

                moveY : String
                moveY = toString (((y0 + y1) // 2 - thickness // 2) - 25) ++ "px"   -- why this is off by 25 pixels? no one knows

                translate : String
                translate = "translate(" ++ moveX ++ "," ++ moveY ++ ")"
            in
            Html.div
                [ HtmlAttrs.style
                    [ ("transform", translate ++ " " ++ rotate)
                    , ("-webkit-transform", translate ++ " " ++ rotate)
                    , ("backgroundColor", color)
                    , ("width", toString width ++ "px")
                    , ("height", toString thickness ++ "px")
                    ]
                ] []
    in
    Dict.get child model.conversation
    |> Maybe.map childPos
    |> Maybe.map (drawLine "#0000ff" 5 (x, y))
    |> Maybe.map (\html -> [html])
    |> Maybe.withDefault []


viewNode : Signal.Address Action -> String -> Conversation.Node -> Html
viewNode address key node =
    case node of 
        Conversation.Talking speech ->
            Speech.view 
                (Speech.Events 
                    (Signal.forwardTo address (ModifySpeech key)) 
                    (Signal.forwardTo address (not >> SetFocus))
                    (Signal.forwardTo address (always (RemoveNode key)))
                    (Signal.forwardTo address (always (StartParenting (PSpeech key))))
                    (Signal.forwardTo address (always (SetChild key))))
                speech

        Conversation.Asking questions ->
            Questions.view 
                (Questions.Events
                    (Signal.forwardTo address (ModifyQuestions key)) 
                    (Signal.forwardTo address (not >> SetFocus))
                    (Signal.forwardTo address (always (RemoveNode key)))
                    (Signal.forwardTo address (PQuestion key >> StartParenting))
                    (Signal.forwardTo address (always (SetChild key))))
                questions


view : Signal.Address Action -> Model -> Html
view address model =
    let isSpeech : Conversation.Node -> Maybe Speech.Model
        isSpeech node = 
            case node of
                Conversation.Talking speech -> Just speech
                _ -> Nothing

        isQuestions : Conversation.Node -> Maybe Questions.Model
        isQuestions node =
            case node of
                Conversation.Asking questions -> Just questions
                _ -> Nothing

        speechConnections : List Html
        speechConnections =
            Dict.values model.conversation
            |> List.filterMap isSpeech
            |> List.map (\speech -> 
                List.indexedMap (\index child ->
                        Speech.childBtnPosition speech index
                        |> viewConnection model child) 
                    speech.children
                |> List.concat)
            |> List.concat

        questionConnections : List Html
        questionConnections =
            Dict.values model.conversation
            |> List.filterMap isQuestions
            |> List.map (\questions ->
                Array.map (\question ->
                    List.indexedMap (\index child ->
                            Question.childBtnPosition question index
                            |> Questions.toParentSpace questions
                            |> viewConnection model child)
                        question.children
                    |> List.concat)
                    questions.questions
                |> Array.toList
                |> List.concat)
            |> List.concat
    in
    Dict.map (viewNode address) model.conversation
    |> Dict.values
    |> ((++) (speechConnections ++ questionConnections))
    |> Html.div []


