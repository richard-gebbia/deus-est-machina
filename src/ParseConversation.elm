module ParseConversation where

import Conversation
import Dict exposing (Dict)
import Json.Decode exposing (..)
import List
import String

type alias Node = Conversation.ConversationNode

{-

Example Json of a "Speech":

    ava1: {
        name: "Ava",
        text: [
            "Hello",
            "My name is Ava"
        ],
        children: [
            "ava2",
            "sebastian1",
            "sophie3"
        ]
    }

-}

speech : Decoder Node
speech =
    object3 Conversation.Speech
        ("name" := string)
        ("text" := list string)
        ("children" := list string)
    |> map Conversation.Talking

{-

Example Json of a "Questions":

    questions2: [
        {
            text: [
                "How could you possibly",
                "be so stupid?"
            ],
            children: [
                "ava3",
                "sophie9"
            ]
        },
        {
            text: [
                "No seriously,",
                "what is your problem?"
            ],
            children: [
                "sebastian2"
            ]
        },
        {
            text: [
                "This isn't a question?"
            ],
            children: []
        }
    ]

-}

question : Decoder Conversation.Question
question =
    object2 Conversation.Question
        ("text" := list string)
        ("children" := list string)


questions : Decoder Node
questions =
    list question
    |> map Conversation.Asking


conversationNode : Decoder Node
conversationNode =
    oneOf
        [ speech
        , questions
        ]

conversation : Decoder (Dict String Node)
conversation =
    dict conversationNode

{-

Other rules that will be verified:

    1.  The keys can be any string you want. If you want a speech or a
        question to lead to another speech or question, the key of the second
        speech or question must be in the "children" array of the first.

    2.  The "name" field of a speech must be one of the four chosen names:
        "Ava", "Gavin", "Sebastian", or "Sophie"

    3.  Questions cannot have another questions as a child.

    4.  If a speech should lead to a questions, the key of that questions should
        be the one child in its "children" array.

-}

verifyAllKeysValid : Dict String Node -> Result String (Dict String Node)
verifyAllKeysValid dict = 
    let getChildren node =
            case node of
                Conversation.Talking speech ->
                    speech.children
                Conversation.Asking questions ->
                    List.concatMap .children questions
        children = 
            Dict.values dict |> List.concatMap getChildren
        errorMessage name = 
            "Child \"" ++ name ++ "\" does not map to anything!"
        check name dict =
            if Dict.member name dict then
                Result.Ok dict
            else
                Result.Err (errorMessage name)
        foldCheck name rDict =
            Result.andThen rDict (check name) 
    in
    List.foldr foldCheck (Result.Ok dict) children

verifyValidNames : Dict String Node -> Result String (Dict String Node)
verifyValidNames dict =
    let getName node =
            case node of 
                Conversation.Talking speech -> speech.name
                Conversation.Asking questions -> ""    
        names = 
            Dict.values dict
            |> List.map getName
            |> List.filter (not << String.isEmpty)
        correctNames =
            ["Ava", "Gavin", "Sebastian", "Sophie"]
            |> List.foldr (\name dict -> Dict.insert name () dict) Dict.empty
        intercalate x list =
            List.intersperse x list |> String.concat
        errorMessage name =
            "Name \"" ++ name ++ "\" is not one of " ++ 
                (intercalate ", " (Dict.keys correctNames))
        check name dict =
            if Dict.member name correctNames then
                Result.Ok dict
            else
                Result.Err (errorMessage name) 
        foldCheck name rDict =
            Result.andThen rDict (check name)
    in
    List.foldr foldCheck (Result.Ok dict) names

--verifyQuestionsLeadToSpeech : Dict String Node -> Result String (Dict String Node)

--verifyLeadToOneQuestion : Dict String Node -> Result String (Dict String Node)

