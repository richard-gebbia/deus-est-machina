module Main where

import Color
import Conversation exposing (Conversation)
import ConversationGen
import Debug
import Dict exposing (Dict)
import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Interjection exposing (Interjection)
import List
import Mouse
import PortraitBox
import PortraitBoxGen
import QuestionList
import QuestionListGen
import Signal exposing (Signal, (<~), (~))
import Text
import Textbox
import TextboxGen
import TextboxList
import TextboxListGen
import Time

-- Model 

type alias Model = 
    {
        conversation: Conversation,
        previous: String,
        portraitBox: PortraitBox.Model,
        textboxList: TextboxList.Model,
        questionList: QuestionList.Model,
        waitToShowQuestions: Bool
    }

-- Action

type Action 
    = FinishTextboxOrShowQuestion
    | FinishTextbox
    | ShowQuestion
    | Advance String
    | ChooseQuestion (List String)
    | Tick Float

-- Update

update : Action -> Model -> Model
update action model = 
    case action of
        Advance name -> 
            advance name model
        FinishTextboxOrShowQuestion -> 
            if model.waitToShowQuestions then
                showQuestions model
            else
                updateTextboxList action model
        ChooseQuestion _ -> model   -- TODO : IMPLEMENT THIS !!! (when I add questions)
        Tick dt -> 
            updateTextboxList action model


getInterjection : Conversation -> String -> String -> Interjection
getInterjection conversation previous name =
    let responseDict = Conversation.getChildrenByName conversation
        getName (Conversation.Talking speech) = speech.name
        currentName = 
            Dict.get previous conversation.graph 
            |> Maybe.map getName 
            |> Maybe.withDefault ""
        continueOrExclaim = 
            if name == currentName then 
                Interjection.Continuation 
            else 
                Interjection.Exclamation
    in
    if Dict.member name responseDict then 
        continueOrExclaim 
    else 
        Interjection.Quiet


getInterjections : Model -> List Interjection
getInterjections model =
    List.map 
        (getInterjection model.conversation model.previous) 
        (PortraitBox.names model.portraitBox)


startingInterjections : List String -> Dict String String -> List Interjection
startingInterjections names starters =
    let interjection name =
        if Dict.member name starters then
            Interjection.Exclamation
        else
            Interjection.Quiet
    in
    List.map interjection names


advance : String -> Model -> Model
advance name model =
    let (newConversation, maybeNode) = 
            Conversation.advance name model.conversation
    in
    case maybeNode of 
        Just (Conversation.Talking speech) ->
            { model | 
                previous <- model.conversation.current,
                conversation <- newConversation,
                textboxList <- 
                    TextboxList.update
                        (TextboxList.AddTextbox speech.name speech.text)
                        model.textboxList |> fst,
                portraitBox <- 
                    PortraitBox.update (PortraitBox.LetThemSpeak) model.portraitBox
            }
        _ ->
            model



readyToAdvance : Model -> Model
readyToAdvance model =
    if Conversation.areQuestionsComingUp model.conversation then
        { model |
            waitToShowQuestions <- True
        }
    else
        { model |
            portraitBox <- 
                PortraitBox.update
                    (PortraitBox.SetInterjections (getInterjections model))
                    model.portraitBox
        }


updateTextboxList : Action -> Model -> Model
updateTextboxList action model =
    let tblistAction = 
            case action of
                FinishTextboxOrShowQuestion -> 
                    TextboxList.FinishCurrentTextbox

                Tick dt ->
                    TextboxList.Tick <| Time.inSeconds dt
        (textboxList, request) = 
            TextboxList.update tblistAction model.textboxList
    in
    case request of
        Just (TextboxList.ReadyForNewTextboxes) ->
            readyToAdvance model 
            |> \model -> { model | textboxList <- textboxList }

        _ -> 
            { model | textboxList <- textboxList }


showQuestions : Model -> Model
showQuestions model =
    let (newConversation, maybeQuestions) = 
            Conversation.advanceToQuestions model.conversation
    in
    case maybeQuestions of
        Just questions ->
            { model |
                questionList <-
                    QuestionList.update 
                        (QuestionList.ShowQuestions 
                            (List.map .text questions)
                            (List.map .children questions))
                        model.questionList,
                textboxList <- 
                    TextboxList.update TextboxList.Hide model.textboxList 
                    |> fst
            }
        _ ->
            model

-- View

view : Signal.Address Action -> Model -> List Collage.Form
view address model = 
    let pbEventToAction (PortraitBox.OnPortraitClick name) =
            Advance name

        qlEventToAction (QuestionList.ChooseQuestion children) =
            ChooseQuestion children
    in
    TextboxList.view model.textboxList ++
    QuestionList.view (Signal.forwardTo address qlEventToAction) model.questionList ++
    PortraitBox.view (Signal.forwardTo address pbEventToAction) model.portraitBox

-- Events

actions : Signal.Mailbox Action
actions = Signal.mailbox (Tick 0)


signals : Signal Action
signals =
    Signal.mergeMany
    (actions.signal ::
    [
        (always FinishTextboxOrShowQuestion) <~ Mouse.clicks,
        Tick <~ Time.fps 60
    ])


initModel : Model 
initModel =
    let conversation = ConversationGen.genConversation
        portraitBox = PortraitBoxGen.genPortraitBox 0 -300
    in
    {
        conversation = conversation,
        previous = "",
        portraitBox = 
            portraitBox 
            |> PortraitBox.update 
                (PortraitBox.SetInterjections
                    (startingInterjections 
                        (PortraitBox.names portraitBox) 
                        (Conversation.getChildrenByName conversation))),
        textboxList = TextboxListGen.genTextboxList,
        questionList = QuestionListGen.genQuestionList,
        waitToShowQuestions = False
    }
    

main : Signal Element
main =
    Signal.foldp update initModel signals
    |> Signal.map (view actions.address)
    |> Signal.map (Collage.collage 1024 768) 