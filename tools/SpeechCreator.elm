import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing ((::))
import Text

-- Model

type alias Speech =
  { name: String
  , text: String
  , children: List String
  }
  
  
type alias Question =
  { text: String
  , children: List String
  }
  
  
type alias Questions = List Question
  

-- Action
-- Update
-- View

viewSpeech : Speech -> Html
viewSpeech speech =
  let isSelected name = 
    [value name, selected (name == speech.name)]
  in
  div 
    [ style 
      [ ("backgroundColor", "rgb(200,200,255)")
      , ("width", "150px")
      , ("height", "150px")
      , ("textAlign", "center")
      , ("borderStyle", "solid")
      , ("borderWidth", "1px")
      ]
    ]
    [ b [] [text "Speech"] 
    , br [] []
    , select [] 
      [ option (isSelected "Sophie") [text "Sophie"]
      , option (isSelected "Gavin") [text "Gavin"]
      , option (isSelected "Ava") [text "Ava"]
      , option (isSelected "Sebastian") [text "Sebastian"]
      ]
    , br [] []
    , textarea 
      [ style [("height", "100px")] 
      , value speech.text ] 
      []
    ]
    
    
viewQuestion : Question -> Html
viewQuestion question =
  div 
    [ style
      [ ("backgroundColor", "rgb(200,255,200)")
      , ("width", "150px")
      , ("borderStyle", "solid")
      , ("borderWidth", "1px")
      ]
    ]
    [ b [] [text "Question"]
    , br [] []
    , textarea [] []
    ]
  

viewQuestions : Questions -> Html
viewQuestions questions =
  let question q = 
    [br [] [], viewQuestion q]
  in
  div
    [ style
      [ ("backgroundColor", "rgb(255,255,200)")
      , ("width", "150px")
      , ("borderStyle", "solid")
      , ("borderWidth", "1px")
      ]
    ]
    (b [] [text "Questions"] :: List.concatMap question questions)


-- Events

-- Main

main : Html
main = 
  div []
    [ viewSpeech
      { name = "Sebastian"
      , text = "Hello!"
      , children = []
      }
    , viewQuestions
      [ { text = ""
        , children = []
        }
      , { text = ""
        , children = []
        }
      ]
    ]
  