module Conversation where

import Debug
import Dict exposing (Dict)
import List exposing ((::))
import Maybe exposing (Maybe)

type alias Speech = 
    {
        name: String,
        text: List String,
        children: List String
    }


type alias SpeechInfo =
    {
        name: String,
        text: List String
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

-- Utility Functions

(!!) : List a -> Int -> Maybe a
list !! index = List.drop index list |> List.head


(|?>) : Maybe a -> (a -> b) -> Maybe b
m |?> f = Maybe.map f m


(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = Maybe.andThen m f


maybeAnd : (Maybe a, Maybe b) -> Maybe (a, b)
maybeAnd m =
    case m of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing


maybeEvery : List (Maybe a) -> Maybe (List a)
maybeEvery ms = 
    let allValid m mlist = 
            case (m, mlist) of
                (Just x, Just xs) -> 
                    Just (x :: xs)
                _ ->
                    Nothing
    in
    List.foldr allValid (Just []) ms


justNode : (ConversationNode -> Bool) -> ConversationNode -> Maybe ConversationNode
justNode pred node = 
    if pred node then
        Maybe.Just node
    else
        Nothing


isSpeech : ConversationNode -> Bool
isSpeech node = 
    case node of
        Talking _ -> True
        _ -> False


isQuestion : ConversationNode -> Bool
isQuestion node = 
    case node of 
        Asking _ -> True
        _ -> False


asSpeech : Conversation -> Maybe Speech
asSpeech conversation =
    Dict.get conversation.current conversation.graph
    >>= justNode isSpeech
    |?> \(Talking speech) -> speech


withoutChildren : Speech -> SpeechInfo
withoutChildren speech = {speech - children}


asQuestions : Conversation -> Maybe Questions
asQuestions conversation =
    Dict.get conversation.current conversation.graph
    >>= justNode isQuestion
    |?> \(Asking questions) -> questions


speakerChildren : Conversation -> Maybe (List String)
speakerChildren conversation =
    asSpeech conversation
    |?> \speech -> speech.children


questionText : Questions -> List (List String)
questionText questions =
    List.map .text questions


questionChildren : Int -> Questions -> Maybe (List String)
questionChildren index questions =
    (questions !! index)
    |?> .children


getNameFromKey : Conversation -> String -> Maybe String
getNameFromKey conversation key =
    Dict.get key conversation.graph
    >>= justNode isSpeech
    |?> (\(Talking node) -> Just node.name)
    |> Maybe.withDefault Nothing   


doesChildHaveRequestedName : Conversation -> String -> String -> Bool
doesChildHaveRequestedName conversation name key =
    Dict.get key conversation.graph
    >>= justNode isSpeech
    |?> (\(Talking node) -> node.name == name)
    |> Maybe.withDefault False


-- Actual useful functions

chooseSpeaker : String -> Conversation -> Maybe (Conversation, SpeechInfo)
chooseSpeaker name conversation =
    speakerChildren conversation
    |?> List.filter (doesChildHaveRequestedName conversation name)
    >>= List.head
    |?> (\key -> { conversation | current <- key })
    |> (\conv -> (conv, (conv >>= asSpeech) |?> withoutChildren ))
    |> maybeAnd


areQuestionsComingUp : Conversation -> Bool
areQuestionsComingUp conversation =
    speakerChildren conversation
    >>= List.head
    >>= (\key -> Dict.get key conversation.graph)
    |?> isQuestion
    |> Maybe.withDefault False


getResponderNames : Conversation -> Maybe (List String)
getResponderNames conversation =
    speakerChildren conversation
    |?> List.map (getNameFromKey conversation)
    |?> maybeEvery
    |> Maybe.withDefault Nothing


getQuestions : Conversation -> Maybe (Conversation, List (List String))
getQuestions conversation = 
    speakerChildren conversation                   
    >>= List.head 
    |?> (\key -> { conversation | current <- key })
    |> (\conv -> (conv, (conv >>= asQuestions) |?> questionText))     
    |> maybeAnd 


chooseQuestion : Int -> Conversation -> Maybe (List String)
chooseQuestion index conversation =
    asQuestions conversation                  
    >>= (\questions -> questions !! index)    
    |?> .children                             
    |?> List.map (getNameFromKey conversation)
    >>= maybeEvery


getQuestionText : Int -> Conversation -> Maybe (List String)
getQuestionText index conversation =
    asQuestions conversation
    >>= (\questions -> questions !! index)
    |?> .text


chooseQuestionResponder : Int -> String -> Conversation -> Maybe (Conversation, SpeechInfo)
chooseQuestionResponder index name conversation =
    asQuestions conversation
    >>= questionChildren index
    |?> List.filter (doesChildHaveRequestedName conversation name)
    >>= List.head
    |?> (\key -> { conversation | current <- key })
    |> (\conv -> (conv, (conv >>= asSpeech) |?> withoutChildren ))
    |> maybeAnd
