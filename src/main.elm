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
        textboxList: TextboxList.Model
    }

-- Action

type Action 
    = FinishTextbox
    | Advance String
    | ChooseQuestion String
    | Tick Float

-- Update

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
    let (newConversation, didChange) = 
            Conversation.advance name model.conversation
    in
    if didChange then
        { model | 
            previous <- model.conversation.current,
            conversation <- newConversation,
            textboxList <- 
                TextboxList.update
                    (TextboxList.AddTextbox 
                        (Conversation.currentName newConversation
                        |> Maybe.withDefault "") 
                        (Conversation.currentText newConversation
                        |> Maybe.withDefault []))
                    model.textboxList |> fst,
            portraitBox <- 
                PortraitBox.update (PortraitBox.LetThemSpeak) model.portraitBox
        }
    else
        model


update : Action -> Model -> Model
update action model = 
    case action of
        Advance name -> 
            if TextboxList.isReadyForNewTextboxes model.textboxList then
                advance name model
            else
                updateTextboxList FinishTextbox model
        FinishTextbox -> 
            updateTextboxList action model
        ChooseQuestion _ -> model   -- TODO : IMPLEMENT THIS !!! (when I add questions)
        Tick dt -> 
            updateTextboxList action model


readyToAdvance : Model -> Model
readyToAdvance model =
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
                FinishTextbox -> 
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

-- View

view : Model -> List Collage.Form
view model = 
    TextboxList.view model.textboxList ++
    PortraitBox.view model.portraitBox

-- Events

signals : Signal Action
signals =
    let portraitClick (PortraitBox.OnPortraitClick name) = Advance name
    in
    Signal.mergeMany
    [
        portraitClick <~ (PortraitBox.onPortraitClick initModel.portraitBox),
        (always FinishTextbox) <~ Mouse.clicks,
        Tick <~ Time.fps 60
    ]


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
        textboxList = TextboxListGen.genTextboxList
    }
    

main : Signal Element
main =
    Signal.foldp update initModel signals
    |> Signal.map view
    |> Signal.map (Collage.collage 1024 768) 