module ConversationGen where

import Conversation exposing (Conversation)
import Debug
import Dict
import Json.Decode
import List
import Maybe
import ParseConversation
import Result

starter : String
starter = "ava1"

conversation : String
conversation = 
    """
    {
        "ava1": {
            "name": "Ava",
            "text": [
                "Hello, my name is Ava,",
                "and your face is stupid."
            ],
            "children": [
                "ava2",
                "gavin1"
            ]
        },
        "ava2": {
            "name": "Ava",
            "text": [
                "No seriously.",
                "What are you going to do about it?"
            ],
            "children": [
                "ava3",
                "sophie1"
            ]
        },
        "ava3": {
            "name": "Ava",
            "text": [
                "I can't keep my eyes open."
            ],
            "children": []
        },
        "gavin1": {
            "name": "Gavin",
            "text": [
                "This sentence should end with a period?"
            ],
            "children": []
        },
        "sophie1": {
            "name": "Sophie",
            "text": [
                "Can I write a haiku?",
                "Oh I already fucked up",
                "Refrigerator anyway"
            ],
            "children": []
        }
    }
    """


genConversation : Conversation
genConversation =
    let conversationGraph =
            Json.Decode.decodeString ParseConversation.conversation conversation
            |> \result ->
                    case result of
                        Result.Ok dict -> dict
                        Result.Err message -> Debug.log message Dict.empty
        startName node =
            case node of
                Conversation.Talking speech -> Just speech.name
                _ -> Nothing
        starterNode = 
            Conversation.Talking 
            {
                name = Dict.get starter conversationGraph `Maybe.andThen` startName |> Maybe.withDefault "",
                text = [],
                children = [starter]
            }
    in
    Debug.log "conversation" {
        graph = 
            conversationGraph
            |> Dict.insert "starter" starterNode,
        current = "starter"
    }
