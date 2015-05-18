module QuestionListGen where

import QuestionList
import TextboxGen

genQuestionList : QuestionList.Model
genQuestionList =
    {
        questions = [],
        firstQuestionPosition = (10, 10),
        questionStride = (0, -140),
        questionFrame = TextboxGen.genTextbox "Question"
    }
