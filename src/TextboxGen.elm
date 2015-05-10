module TextboxGen where

import Color
import Textbox

genTextbox : String -> List String -> (Float, Float) ->  Textbox.Model
genTextbox name text (x, y) =
    {
        name = name,
        text = text,
        background = 
            {
                x = -10 + x,
                y = y,
                width = 984,
                height = 120,
                imageName = "img/Text" ++ name ++ ".png"
            },
        portrait = 
            {
                x = -442 + x,
                y = -7 + y,
                width = 100,
                height = 100,
                imageName = "img/" ++ name ++ ".png"
            },
        nameStyle = 
            {
                typeface = ["Helvetica", "Arial", "sans serif"],
                height = Just 20,
                color = Color.black,
                bold = True,
                italic = False,
                line = Nothing
            },
        nameX = -380 + x,
        nameY = 50 + y,
        textStyle = 
            {
                typeface = ["Helvetica", "Arial", "sans serif"],
                height = Just 18,
                color = Color.black,
                bold = False,
                italic = False,
                line = Nothing
            },
        textX = -378 + x,
        textY = 23 + y,
        lettersPerSecond = 15,
        elapsedTime = 0
    }
