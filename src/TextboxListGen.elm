module TextboxListGen where

import Dict
import MoveByAnimation
import TextboxGen
import TextboxList

genTextboxList : TextboxList.Model
genTextboxList = 
    {
        textboxes = [],
        animation = MoveByAnimation.linearAnimation 0 140 0.1,
        enterPosition = (10, -280),
        textboxFrames = Dict.empty 
                        |> Dict.insert "Sophie"    (TextboxGen.genTextbox "Sophie")
                        |> Dict.insert "Gavin"     (TextboxGen.genTextbox "Gavin")
                        |> Dict.insert "Ava"       (TextboxGen.genTextbox "Ava")
                        |> Dict.insert "Sebastian" (TextboxGen.genTextbox "Sebastian"),
        hidden = False
    }
