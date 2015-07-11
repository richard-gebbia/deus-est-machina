module QuestionListGen where

import QuestionList
import TextboxGen

genQuestionList : QuestionList.Model
genQuestionList =
    {
        questions = [],
        firstQuestionPosition = (0, 200),
        questionStride = (0, -87),
        questionFrame = TextboxGen.genTextbox "Question" True
    }
