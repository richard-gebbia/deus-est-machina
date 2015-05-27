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
        current: String,
        chosenQuestionIndex: Maybe Int
    }


(!!) : List a -> Int -> Maybe a
(!!) list index =
    if index >= 0 then
        List.head (List.drop index list)
    else
        Nothing


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


currentNode : Conversation -> Maybe ConversationNode
currentNode conversation =
    Dict.get conversation.current conversation.graph


advance : String -> Conversation -> (Conversation, Maybe ConversationNode)
advance name conversation =
    let validNames = getChildrenByName conversation
        node =  Dict.get name validNames
                `andThen` (\key -> Dict.get key conversation.graph)
    in
    Dict.get name validNames
    |> Maybe.map (\key -> ({ conversation | current <- key }, node))
    |> Maybe.withDefault (conversation, Nothing)


chooseQuestion : Int -> Conversation -> Conversation
chooseQuestion index conversation =
    let node = currentNode conversation
        nullCase =
            { conversation | chosenQuestionIndex <- Nothing }
    in
    case node of 
        Just (Asking questions) -> 
            if index >= 0 && index < List.length questions then
                { conversation |
                    chosenQuestionIndex <- Just index
                }
            else 
                nullCase

        _ ->
            nullCase


getChildrenByName : Conversation -> Dict String String
getChildrenByName conversation =
    let addToDict childKey dict = 
            Dict.get childKey conversation.graph
            `andThen` justNode isSpeech
            |> Maybe.map (\(Talking node) -> Dict.insert node.name childKey dict)
            |> Maybe.withDefault dict
        allChildren questions =
            List.concatMap .children questions
        getChildren node =
            case node of
                Talking speech ->
                    speech.children
                Asking questions ->
                    case conversation.chosenQuestionIndex of
                        Just i -> 
                            case (questions !! i) of
                                Just question ->
                                    question.children
                                Nothing ->
                                    allChildren questions
                        Nothing ->
                            allChildren questions
    in
    Dict.get conversation.current conversation.graph
    |> Maybe.map getChildren
    |> Maybe.map (List.foldl addToDict Dict.empty)
    |> Maybe.withDefault Dict.empty


areQuestionsComingUp : Conversation -> Bool
areQuestionsComingUp conversation =
    (Dict.get conversation.current conversation.graph
    `andThen` justNode isSpeech
    |> Maybe.map (\(Talking node) -> node.children))
    `andThen` List.head
    `andThen` (\key -> Dict.get key conversation.graph)
    |> Maybe.map isQuestion
    |> Maybe.withDefault False
