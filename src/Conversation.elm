module Conversation where

import Debug
import Dict exposing (Dict)
import List exposing ((::))
import Maybe exposing (Maybe, andThen)

type alias Speech = 
    {
        name: String,
        text: List String,
        children: List String
    }


type alias Question =
    {
        text: List String,
        children: List String
    }


type alias Questions = List Question


type ConversationNode 
    = Talking Speech
    | Asking Questions


type alias Conversation =
    {
        graph: Dict String ConversationNode,
        current: String
    }


fromJust : Maybe a -> a
fromJust m =
    case m of 
        Just x -> x
        Nothing -> Debug.crash ("Error: Supplied a Nothing to fromJust!")


isSpeech : ConversationNode -> Bool
isSpeech node =
    case node of
        Talking _ -> True
        Asking _ -> False


isQuestion : ConversationNode -> Bool
isQuestion node =
    case node of
        Talking _ -> False
        Asking _ -> True


justNode : (ConversationNode -> Bool) -> ConversationNode -> Maybe ConversationNode
justNode predicate node =
    if predicate node then 
        Just node
    else
        Nothing


currentName : Conversation -> Maybe String
currentName conversation = 
    Dict.get conversation.current conversation.graph
    `andThen` justNode isSpeech
    |> Maybe.map (\(Talking speech) -> speech.name)


currentText : Conversation -> Maybe (List String)
currentText conversation =
    Dict.get conversation.current conversation.graph
    `andThen` justNode isSpeech
    |> Maybe.map (\(Talking speech) -> speech.text)


advance : String -> Conversation -> (Conversation, Bool)
advance name conversation =
    let validNames = getChildrenByName conversation
    in
    Dict.get name validNames
    |> Maybe.map (\key -> ({ conversation | current <- key }, True))
    |> Maybe.withDefault (conversation, False)


getChildrenByName : Conversation -> Dict String String
getChildrenByName conversation =
    let addToDict childKey dict = 
            Dict.get childKey conversation.graph
            `andThen` justNode isSpeech
            |> Maybe.map (\(Talking node) -> Dict.insert node.name childKey dict)
            |> Maybe.withDefault dict
        getChildren node =
            case node of
                Talking speech ->
                    speech.children
                Asking questions ->
                    List.concatMap .children questions
    in
    Dict.get conversation.current conversation.graph
    |> fromJust
    |> getChildren
    |> List.foldl addToDict Dict.empty