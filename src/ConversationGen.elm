module ConversationGen where

import Conversation exposing (Conversation)
import Debug
import Dict
import Maybe
import List

conversation : List (String, Conversation.Speech)
conversation = 
    [
        (
            "ava1",
            {
                name = "Ava",
                text = 
                    [
                        "Hello!",
                        "I'm Ava, and your face is stupid!",
                        "What are you going to do about it?"
                    ],
                children = 
                    [
                        "gavin1",
                        "ava2"
                    ]
            }
        ),
        (
            "gavin1",
            {
                name = "Gavin",
                text = 
                    [
                        "This sentence should end in a period?"
                    ],
                children = []
            }
        ),
        (
            "ava2",
            {
                name = "Ava",
                text = 
                    [
                        "No, seriously.",
                        "It's a huge problem."
                    ],
                children = ["ava3", "sophie1"]
            }
        ),
        (
            "ava3",
            {
                name = "Ava",
                text =
                    [
                        "I can't keep my eyes open."
                    ],
                children = []
            }
        ),
        (
            "sophie1",
            {
                name = "Sophie",
                text =
                    [
                        "Can I write a haiku?",
                        "Oh I already fucked up.",
                        "Refrigerator anyway"
                    ],
                children = []
            }
        )
    ]


genConversation : Conversation
genConversation =
    let starter = 
        {
            name = List.head conversation |> Maybe.map snd |> Maybe.map .name |> Maybe.withDefault "",
            text = [],
            children = [List.head conversation |> Maybe.map fst |> Maybe.withDefault ""]
        }
    in
    {
        graph = Dict.fromList conversation |> Dict.insert "starter" starter,
        current = "starter"
    }
