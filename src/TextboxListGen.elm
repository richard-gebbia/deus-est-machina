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
                        |> Dict.insert "Sophie"    (TextboxGen.genTextbox "Sophie"    False)
                        |> Dict.insert "Gavin"     (TextboxGen.genTextbox "Gavin"     False)
                        |> Dict.insert "Ava"       (TextboxGen.genTextbox "Ava"       False)
                        |> Dict.insert "Sebastian" (TextboxGen.genTextbox "Sebastian" False),
        hidden = False
    }
