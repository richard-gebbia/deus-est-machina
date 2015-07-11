module TextboxGen where

import Color
import Textbox

genTextbox : String -> Bool -> List String -> (Float, Float) ->  Textbox.Model
genTextbox name clickable text (x, y) =
    {
        name = name,
        text = text,
        background = 
            {
                x = x,
                y = y,
                width = 631,
                height = 77,
                imageName = "img/" ++ name ++ "DialogueBox.png"
            },
        portrait = 
            {
                x = -276 + x,
                y = -3 + y,
                width = 64,
                height = 64,
                imageName = "img/" ++ name ++ ".png"
            },
        nameStyle = 
            {
                typeface = ["Helvetica", "Arial", "sans serif"],
                height = Just 14,
                color = Color.black,
                bold = True,
                italic = False,
                line = Nothing
            },
        nameX = -237 + x,
        nameY = 35 + y,
        textStyle = 
            {
                typeface = ["Helvetica", "Arial", "sans serif"],
                height = Just 12,
                color = Color.black,
                bold = False,
                italic = False,
                line = Nothing
            },
        textX = -236 + x,
        textY = 19 + y,
        lettersPerSecond = 15,
        elapsedTime = 0,
        clickable = clickable
    }
