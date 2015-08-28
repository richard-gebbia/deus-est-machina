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
starter = "0"

testConversation : String
testConversation = 
    """
    {
        "start": {
            "name": "Ava",
            "text": [
                "Test"
            ],
            "children": []
        }
    }
    """


genConversation : String -> Conversation
genConversation conversationString =
    let conversationGraph =
            Json.Decode.decodeString ParseConversation.conversation conversationString
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
    {   -- TODO : Add verification
        graph = 
            conversationGraph
            |> Dict.insert "starter" starterNode,
        current = "starter"
    }
