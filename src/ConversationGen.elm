module ConversationGen where

import Conversation exposing (Conversation)
import Debug
import Dict exposing (Dict)
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
    let conversationGraph : Dict String Conversation.ConversationNode
        conversationGraph =
            Json.Decode.decodeString ParseConversation.conversation conversationString
            |> \result ->
                    case result of
                        Result.Ok dict -> dict
                        Result.Err message -> Debug.log message Dict.empty

        startName : Conversation.ConversationNode -> Maybe String                        
        startName node =
            case node of
                Conversation.Talking speech -> Just speech.name
                _ -> Nothing

        starterNode : Conversation.ConversationNode                
        starterNode = 
            Conversation.Talking 
            {
                name = Dict.get starter conversationGraph `Maybe.andThen` startName |> Maybe.withDefault "",
                text = [],
                children = [starter]
            }
    in
    {
        graph = 
            conversationGraph
            |> Dict.insert "starter" starterNode,
        current = "starter"
    }
