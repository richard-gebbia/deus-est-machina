module DeusEstMachina where

import Conversation exposing (Conversation)
import ConversationGen
import Debug
import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Interjection exposing (Interjection)
import Maybe exposing (andThen)
import Mouse
import PortraitBox
import PortraitBoxGen
import QuestionList
import QuestionListGen
import Signal exposing (Signal, (<~))
import TextboxList
import TextboxListGen
import Time

-- Model 

type alias ModelData =
    {
        conversation: Conversation,
        portraitBox: PortraitBox.Model,
        previous: Maybe String,
        questionList: QuestionList.Model,
        textboxList: TextboxList.Model,
        gameStateUpdate: Action -> Model -> Model
    }

type Model = Model ModelData

unwrap : Model -> ModelData
unwrap (Model data) = data
    

-- Action

type Action
    = Click
    | ChooseSpeaker String
    | ChooseQuestion Int
    | Tick Float


-- Update

advance : ModelData -> String -> (Conversation, Conversation.SpeechInfo) -> ModelData
advance modelData name (newConversation, speech) =
    { modelData |
        conversation <- newConversation,
        previous <- Just name,
        portraitBox <- 
            PortraitBox.update 
                (PortraitBox.LetThemSpeak name) 
                modelData.portraitBox,
        textboxList <- 
            TextboxList.update
                (TextboxList.AddTextbox speech.name speech.text)
                modelData.textboxList |> fst,
        gameStateUpdate <- waitingForTextboxToFinish
    }


choosingSpeaker : Action -> Model -> Model
choosingSpeaker action model =
    let modelData = unwrap model
    in
    case action of 
        ChooseSpeaker name ->
            Conversation.chooseSpeaker name (unwrap model).conversation
            |> Maybe.map (advance (unwrap model) name)
            |> Maybe.map Model
            |> Maybe.withDefault model

        Tick dt ->
            { modelData |
                portraitBox <- 
                    PortraitBox.update (PortraitBox.Tick dt) modelData.portraitBox
            } |> Model

        _ -> 
            model 


waitingForTextboxToFinish : Action -> Model -> Model
waitingForTextboxToFinish action model =
    let onClick modelData =
            { modelData |
                portraitBox <- 
                    if Conversation.areQuestionsComingUp modelData.conversation then
                        PortraitBox.update 
                            PortraitBox.ThePlayerIsSpeaking
                            modelData.portraitBox
                    else
                        Conversation.getResponderNames modelData.conversation
                        |> Maybe.map (\names -> 
                            PortraitBox.update 
                                (PortraitBox.SetResponders names)
                                modelData.portraitBox)
                        |> Maybe.withDefault modelData.portraitBox,
                textboxList <- 
                    (TextboxList.update 
                        TextboxList.FinishCurrentTextbox 
                        modelData.textboxList |> fst),
                gameStateUpdate <-
                    if Conversation.areQuestionsComingUp modelData.conversation then
                        waitingToShowQuestions
                    else
                        choosingSpeaker
            }

        onTick dt modelData =
            TextboxList.update (TextboxList.Tick dt) modelData.textboxList
            |> \(tblist, request) ->
                case request of
                    Just (TextboxList.ReadyForNewTextboxes) ->
                        onClick modelData

                    _ -> 
                        { modelData |
                            textboxList <- tblist
                        }
    in
    case action of
        Click -> 
            onClick (unwrap model) |> Model

        Tick dt ->
            onTick dt (unwrap model) |> Model

        _ -> 
            model


waitingToShowQuestions : Action -> Model -> Model
waitingToShowQuestions action model = 
    let modelData = unwrap model

        onClick (newConversation, questionText) =
            { modelData |
                conversation <- newConversation,
                previous <- Nothing,
                textboxList <-
                    TextboxList.update TextboxList.Hide modelData.textboxList 
                    |> fst,
                questionList <-
                    QuestionList.update
                        (QuestionList.ShowQuestions questionText)
                        modelData.questionList,
                gameStateUpdate <- showingQuestions
            }
    in
    case action of
        Click ->
            Conversation.getQuestions modelData.conversation
            |> Maybe.withDefault (modelData.conversation, [])
            |> onClick 
            |> Model

        _ ->
            model


showingQuestions : Action -> Model -> Model
showingQuestions action model =
    let modelData = unwrap model

        respondToChosenQuestion index (text, names) =
            { modelData | 
                questionList <- 
                    QuestionList.update QuestionList.Hide modelData.questionList,
                textboxList <- 
                    TextboxList.update TextboxList.Show modelData.textboxList 
                    |> fst
                    |> TextboxList.update (TextboxList.AddFullTextbox "Question" text)
                    |> fst,
                portraitBox <-
                    PortraitBox.update 
                        (PortraitBox.SetResponders names)
                        modelData.portraitBox,
                gameStateUpdate <-
                    waitingForQuestionResponse index
            }
    in
    case action of 
        ChooseQuestion index ->
            Conversation.chooseQuestion index modelData.conversation
            |> (\names -> 
                    (
                        Conversation.getQuestionText index modelData.conversation, 
                        names
                    )
                )
            |> (\(text, responders) -> 
                    (Maybe.withDefault [] text, Maybe.withDefault [] responders))
            |> respondToChosenQuestion index
            |> Model

        _ ->
            model


waitingForQuestionResponse : Int -> Action -> Model -> Model
waitingForQuestionResponse index action model = 
    let modelData = unwrap model
    in
    case action of
        Tick dt -> 
            { modelData |
                textboxList <- 
                    TextboxList.update (TextboxList.Tick dt) modelData.textboxList
                    |> fst,
                portraitBox <-
                    PortraitBox.update (PortraitBox.Tick dt) modelData.portraitBox
            } |> Model

        ChooseSpeaker name ->
            Conversation.chooseQuestionResponder index name modelData.conversation
            |> Maybe.map (advance modelData name)
            |> Maybe.map Model
            |> Maybe.withDefault model

        _ -> 
            model


update : Action -> Model -> Model
update action model =
    (unwrap model).gameStateUpdate action model


-- View

view : Signal.Address Action -> Model -> List Collage.Form
view address model = 
    let pbEventToAction (PortraitBox.OnPortraitClick name) =
            ChooseSpeaker name

        qlEventToAction (QuestionList.ChooseQuestion children) =
            ChooseQuestion children
    in
    TextboxList.view (unwrap model).textboxList ++
    QuestionList.view (Signal.forwardTo address qlEventToAction) (unwrap model).questionList ++
    PortraitBox.view (Signal.forwardTo address pbEventToAction) (unwrap model).portraitBox


-- Events

actions : Signal.Mailbox Action
actions = Signal.mailbox (Tick 0)


signals : Signal Action
signals =
    Signal.mergeMany
    (actions.signal ::
    [
        (always Click) <~ Mouse.clicks,
        (Time.inSeconds >> Tick) <~ Time.fps 60
    ])


initModel : Model 
initModel =
    let portraitBox = PortraitBoxGen.genPortraitBox 0 -187
        conversation = ConversationGen.genConversation conversationText
    in
    Model
    {
        conversation = conversation,
        previous = Nothing,
        portraitBox = 
            portraitBox 
            |> PortraitBox.update 
                (PortraitBox.SetResponders 
                    (Conversation.getResponderNames conversation
                    |> Maybe.withDefault [])),
        textboxList = TextboxListGen.genTextboxList,
        questionList = QuestionListGen.genQuestionList,
        gameStateUpdate = choosingSpeaker
    }
    

main : Signal Element
main =
    Signal.foldp update initModel signals
    |> Signal.map (view actions.address)
    |> Signal.map (Collage.collage 640 480)


port conversationText : String
