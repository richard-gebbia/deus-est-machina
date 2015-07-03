conversation = {
    "start": {
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
        "children": ["questions1"]
    },
    "questions1": [
        {
            "text": [
                "A wizard has turned you into a whale.",
                "Is this awesome? Y/N"
            ],
            "children": ["gavin1", "sophie1"]
        },
        {
            "text": [
                "How stupid can one face get?"
            ],
            "children": ["ava3", "sebastian1"]
        },
        {
            "text": [
                "To be, or to have a stupid face?",
                "That is the question."
            ],
            "children": []
        }
    ],
    "sebastian1": {
        "name": "Sebastian",
        "text": [
            "Yo momma's so fat, she's embarrassed that",
            "your face is so stupid."
        ],
        "children": []
    }
}