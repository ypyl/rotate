module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Colors exposing (black, blue, blueFocused, gray, grayColorbackground, grayFocused, lightRed, red, white)
import Config exposing (dayTitleSize, modalSecondColumnWidth, taskSize, weekDayWidth)
import Cron exposing (Cron, WeekDay(..), fromString)
import Date exposing (Date, Unit(..), add, day, format, fromIsoString, isBetween, month, toIsoString, today, year)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, focused, height, htmlAttribute, inFront, layout, none, padding, paddingXY, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onDoubleClick)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Html exposing (Html)
import Html.Attributes exposing (class, type_)
import Humanizer exposing (toString)
import Mappers exposing (cronToString, isCronMatchDate)
import Task
import Time exposing (Month(..))


main : Program ( Int, Int ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        modalWindow =
            case model.editTask of
                Just (EditTask _ t) ->
                    [ grayBackground t |> inFront ]

                Nothing ->
                    []
    in
    layout modalWindow (daysView model)


type alias Model =
    { message : String
    , windowWidth : Int
    , windowHeight : Int
    , today : Date
    , startDate : DateModel
    , tasks : List TaskValue
    , editTask : Maybe EditTask
    }


type alias DateModel =
    { date : Date
    , dateText : String
    }


type EditTask
    = EditTask TaskValue TaskValue


type alias TaskValue =
    { value : String
    , createdDate : Date
    , editDate : Date
    , date : DateModel
    , status : TaskStatus
    , taskType : TaskType
    , error : List String
    }


type TaskType
    = Single
    | CronType CronTaskValue
    | Slide DateModel


type alias CronTaskValue =
    { cron : Cron
    , cronEditValue : String
    , endDate : DateModel
    , cases : List PassedCases
    }


type TaskStatus
    = Done
    | Active
    | Cancel


type alias PassedCases =
    { date : Date
    , status : TaskStatus
    }


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( windowWidth, windowHeight ) =
    ( { message = "hey"
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startDate = initialStartDate 0
      , today = initialDateValue
      , tasks =
            [ { value = "single", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single, error = [] }
            , { value = "slide", date = initialStartDate -5, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Slide (initialStartDate 10), error = [] }
            , { value = "cron", date = initialStartDate -100, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = CronType initialCronTaskValue, error = [] }
            ]
      , editTask = Nothing
      }
    , Task.perform GetToday today
    )


initialCronTaskValue : { cron : Cron, cronEditValue : String, endDate : { date : Date, dateText : String }, cases : List a }
initialCronTaskValue =
    { cron = testCron, cronEditValue = cronToString testCron, endDate = initialStartDate 100, cases = [] }


testCron : Cron
testCron =
    Cron.Cron Cron.Every Cron.Every Cron.Every Cron.Every (Cron.Single (Cron.Atom (Cron.Range Monday Friday)))


initialStartDate : Int -> { date : Date, dateText : String }
initialStartDate delta =
    initDateModel (add Days delta initialDateValue)


initDateModel : Date -> { date : Date, dateText : String }
initDateModel initDate =
    { date = initDate
    , dateText = toIsoString initDate
    }


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2023 Mar 1


type Msg
    = Name String
    | SetWindowWidthHeight Int Int
    | GetToday Date
    | ChangeStartDate String
    | EditTaskDate String
    | SaveTask
    | CancelEdit
    | EditValue String
    | EditTaskMsg TaskValue
    | EditTaskType RadioType
    | EditCronValue String
    | EditTaskEndDate String
    | EditTaskStatus TaskStatus
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | message = name }, Cmd.none )

        SetWindowWidthHeight width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

        GetToday date ->
            let
                startDateModel =
                    model.startDate

                updatedStartDate =
                    { startDateModel
                        | date = date
                        , dateText = Date.toIsoString date
                    }
            in
            ( { model | today = date, startDate = updatedStartDate }
            , Cmd.none
            )

        ChangeStartDate newDate ->
            case fromIsoString newDate of
                Ok newDateParser ->
                    let
                        startDateModel =
                            model.startDate

                        updatedStartDate =
                            { startDateModel
                                | date = newDateParser
                                , dateText = newDate
                            }
                    in
                    ( { model | startDate = updatedStartDate }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SaveTask ->
            case model.editTask of
                Just (EditTask originalTask updatedTask) ->
                    ( { model | tasks = replaceTask originalTask updatedTask model.tasks, editTask = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editTask = Nothing }, Cmd.none )

        EditValue newValue ->
            case model.editTask of
                Just (EditTask originalTask t) ->
                    let
                        updatedTask =
                            { t | value = newValue }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskMsg task ->
            ( { model | editTask = Just (EditTask task task) }, Cmd.none )

        EditTaskType radioType ->
            case model.editTask of
                Just (EditTask originalTask t) ->
                    let
                        updatedTask =
                            { t | taskType = radioTypeToTaskType t.date.date radioType }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditCronValue cronValue ->
            case model.editTask of
                Just (EditTask originalTask t) ->
                    let
                        incorrectCronError =
                            "Incorrect cron value"

                        removeError errors =
                            List.filter (\i -> i /= incorrectCronError) errors

                        addError errors =
                            if List.member incorrectCronError errors then
                                errors

                            else
                                incorrectCronError :: errors

                        newValue =
                            "* * " ++ cronValue

                        updatedTask =
                            case ( fromString newValue, t.taskType ) of
                                ( Ok newCron, CronType cronTaskValue ) ->
                                    { t | taskType = CronType { cronTaskValue | cron = newCron, cronEditValue = cronToString newCron }, error = removeError t.error }

                                ( Ok _, _ ) ->
                                    t

                                ( Err _, CronType cronTaskValue ) ->
                                    { t | error = addError t.error, taskType = CronType { cronTaskValue | cronEditValue = cronValue } }

                                ( Err _, _ ) ->
                                    { t | error = addError t.error }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskDate newDate ->
            case ( model.editTask, fromIsoString newDate ) of
                ( Just (EditTask originalTask t), Ok newParsedDate ) ->
                    let
                        updatedTask =
                            case t.taskType of
                                CronType cronValue ->
                                    { t | date = updatedDateModel t.date, error = isThereStartDateAfterEndDate newParsedDate cronValue.endDate.date t.error }

                                Slide endDate ->
                                    { t | date = updatedDateModel t.date, error = isThereStartDateAfterEndDate newParsedDate endDate.date t.error }

                                Single ->
                                    { t | date = updatedDateModel t.date }

                        updatedDateModel modelToUpdate =
                            { modelToUpdate
                                | date = newParsedDate
                                , dateText = newDate
                            }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditTaskEndDate newDate ->
            case ( model.editTask, fromIsoString newDate ) of
                ( Just (EditTask originalTask t), Ok newParsedDate ) ->
                    case t.taskType of
                        CronType cronValue ->
                            let
                                updatedCronValue =
                                    { cronValue | endDate = updatedDateModel cronValue.endDate }

                                updatedTask =
                                    { t | taskType = CronType updatedCronValue, error = isThereStartDateAfterEndDate t.date.date newParsedDate t.error }

                                updatedDateModel modelToUpdate =
                                    { modelToUpdate
                                        | date = newParsedDate
                                        , dateText = newDate
                                    }
                            in
                            ( { model | editTask = Just (EditTask originalTask updatedTask) }
                            , Cmd.none
                            )

                        Slide endDate ->
                            let
                                updatedTask =
                                    { t | taskType = Slide (updatedDateModel endDate), error = isThereStartDateAfterEndDate t.date.date newParsedDate t.error }

                                updatedDateModel modelToUpdate =
                                    { modelToUpdate
                                        | date = newParsedDate
                                        , dateText = newDate
                                    }
                            in
                            ( { model | editTask = Just (EditTask originalTask updatedTask) }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditTaskStatus newStatus ->
            case model.editTask of
                Just (EditTask originalTask t) ->
                    let
                        updatedTask =
                            { t | status = newStatus }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


isThereStartDateAfterEndDate : Date -> Date -> List String -> List String
isThereStartDateAfterEndDate start end errors =
    let
        startDateAfter =
            "Start date after end date"

        addError error =
            if List.member startDateAfter error then
                error

            else
                startDateAfter :: error

        removeError error =
            List.filter (\i -> i /= startDateAfter) error
    in
    case Date.compare start end of
        GT ->
            addError errors

        _ ->
            removeError errors


replaceTask : TaskValue -> TaskValue -> List TaskValue -> List TaskValue
replaceTask toReplace newTask list =
    let
        check taskValue result =
            if taskValue == toReplace then
                newTask :: result

            else
                taskValue :: result
    in
    if emptyTaskValue toReplace.date.date == toReplace then
        newTask :: list

    else
        List.foldl check [] list |> List.reverse


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w h -> SetWindowWidthHeight w h)


dayTitle : String -> Element msg
dayTitle value =
    el [ paddingXY 0 5, height (px dayTitleSize), centerX ] (text value)


taskValueView : (TaskValue -> List (Element.Attribute Msg)) -> TaskValue -> Element Msg
taskValueView extraAttr task =
    el
        ([ width fill
         , height (px taskSize)
         , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
         , Border.color (rgb255 0 0 0)
         , paddingXY 3 0
         , onDoubleClick (EditTaskMsg task)
         , Html.Attributes.class "no-select" |> Element.htmlAttribute
         ]
            ++ (extraAttr task)
        )
        (text task.value)


emptyTaskValue : Date -> TaskValue
emptyTaskValue date =
    let
        dateModel =
            { date = date
            , dateText = toIsoString date
            }
    in
    { value = "", status = Active, date = dateModel, createdDate = date, editDate = date, taskType = Single, error = [] }


weekDay : Model -> Int -> Element Msg
weekDay model dayDelta =
    let
        atLeastOneEmptyTaskPerDay =
            1

        weekDayDate =
            add Days dayDelta model.startDate.date

        weekDayTasks =
            filteredTaskPerDay model.today weekDayDate model.tasks

        emptyTasks count =
            List.repeat count (taskValueView (\_ -> []) (emptyTaskValue weekDayDate))

        totalCountOfTasksToShow =
            numberOfTaskToShow model.windowHeight

        emptyTaskCount =
            if totalCountOfTasksToShow - List.length weekDayTasks > 0 then
                totalCountOfTasksToShow - List.length weekDayTasks

            else
                atLeastOneEmptyTaskPerDay

        title =
            if dayDelta == 0 then
                dayInput model.startDate ChangeStartDate

            else
                dayTitle (format "E, d MMM y" weekDayDate)

        extraAttr taskValue =
            case (taskValue.taskType, taskValue.status) of
                (Slide _, Active) ->
                    if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                        [ Font.light, Font.color red ]
                    else
                        []

                (CronType _, Active) ->
                    if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                        [ Font.light, Font.color red ]
                    else
                        []

                (_, Done) ->
                    [ Font.strike ]

                (_, Cancel) ->
                    [ Font.strike ]

                _ ->
                    []
    in
    column
        [ Font.color black
        , Border.color black
        , padding 3
        , width (px weekDayWidth)
        ]
        (title :: List.map (taskValueView extraAttr) weekDayTasks ++ emptyTasks emptyTaskCount)


filteredTaskPerDay : Date -> Date -> List TaskValue -> List TaskValue
filteredTaskPerDay today date tasks =
    List.filter (showTaskAtDate today date) tasks


showTaskAtDate : Date -> Date -> TaskValue -> Bool
showTaskAtDate today date taskValue =
    case (taskValue.taskType, taskValue.status) of
        (CronType cronValue, Active) ->
            isBetween today cronValue.endDate.date date && isCronMatchDate cronValue.cron date

        (Slide end, Active) ->
            isBetween today (Date.min today end.date) date

        _ ->
            taskValue.date.date == date


numberOfTaskToShow : Int -> Int
numberOfTaskToShow viewHeight =
    (viewHeight - dayTitleSize) // taskSize - 1


daysView : Model -> Element Msg
daysView model =
    let
        daysToShow count =
            List.range 0 count
                |> List.map (weekDay model)
    in
    row [ width fill, spacing 25, padding 5 ]
        (daysToShow (numberOfWeekDayToShow model.windowWidth))


numberOfWeekDayToShow : Int -> Int
numberOfWeekDayToShow viewWidth =
    viewWidth // weekDayWidth - 1


dayInput : DateModel -> (String -> Msg) -> Element Msg
dayInput model changeEvent =
    row [ alignTop ]
        [ Input.text [ type_ "date" |> htmlAttribute, class "date-input" |> htmlAttribute, Border.width 0, focused [], width modalSecondColumnWidth ]
            { onChange = changeEvent
            , text = model.dateText
            , label = Input.labelHidden "dayInput"
            , placeholder = Nothing
            }
        ]


grayBackground : TaskValue -> Element Msg
grayBackground taskValue =
    el
        [ width fill
        , height fill
        , Background.color grayColorbackground
        ]
        (modalView taskValue)


modalView : TaskValue -> Element Msg
modalView taskValue =
    el
        [ centerX
        , centerY
        , width (px (weekDayWidth * 2))
        , Background.color white
        , Border.rounded 5
        ]
        (editTaskView taskValue)


editTaskView : TaskValue -> Element Msg
editTaskView taskValue =
    let
        twoRowAttr =
            [ spacing 5, width fill ]
    in
    column [ width fill, padding 10, spacing 10 ]
        [ inputTaskStatusView taskValue.status
        , row twoRowAttr [ inputValueView taskValue.value, inputDateView taskValue.date ]
        , row twoRowAttr [ inputTaskTypeView taskValue.taskType, el [ alignRight ] (endDateView taskValue) ]
        , case taskValue.taskType of
            CronType cronTaskValue ->
                if List.isEmpty taskValue.error then
                    row twoRowAttr [ inputCronView cronTaskValue.cronEditValue, cronView (toString cronTaskValue.cron) ]

                else
                    row twoRowAttr [ inputCronView cronTaskValue.cronEditValue, errorView taskValue.error ]

            _ ->
                if List.isEmpty taskValue.error then
                    none

                else
                    row twoRowAttr [ none, errorView taskValue.error ]
        , modalFooter (List.isEmpty taskValue.error |> not)
        ]

inputTaskStatusView : TaskStatus -> Element Msg
inputTaskStatusView taskStatus =
    Input.radioRow [ spacing 10 ]
        { onChange = EditTaskStatus
        , options =
            [ Input.option Active (text "Active")
            , Input.option Cancel (text "Cancelled")
            , Input.option Done (text "Done")
            ]
        , selected = Just taskStatus
        , label = Input.labelHidden "inputTaskStatusView"
        }

endDateView : TaskValue -> Element Msg
endDateView taskValue =
    case taskValue.taskType of
        Single ->
            none

        CronType cronTaskValue ->
            inputEndView cronTaskValue.endDate

        Slide endDate ->
            inputEndView endDate


inputEndView : DateModel -> Element Msg
inputEndView dateModel =
    dayInput dateModel EditTaskEndDate


errorView : List String -> Element msg
errorView errors =
    column [ width modalSecondColumnWidth ]
        (errors |> List.map text |> List.map (\i -> paragraph [ Font.size 14 ] [ i ]))


cronView : String -> Element msg
cronView strCron =
    let
        result =
            case String.split "," strCron of
                _ :: _ :: rest ->
                    String.join "," rest

                _ ->
                    ""
    in
    paragraph [ Font.size 14, width modalSecondColumnWidth ] [ text result ]


inputDateView : DateModel -> Element Msg
inputDateView dateModel =
    dayInput dateModel EditTaskDate


inputValueView : String -> Element Msg
inputValueView value =
    Input.text [ focused [] ]
        { onChange = EditValue
        , text = value
        , placeholder = Nothing
        , label = labelHidden "editValue"
        }


defaultCron : Cron
defaultCron =
    Cron.Cron Cron.Every Cron.Every Cron.Every Cron.Every Cron.Every


type RadioType
    = SingleRadio
    | CronRadio
    | SlideRadio


taskTypeToRadioType : TaskType -> RadioType
taskTypeToRadioType taskType =
    case taskType of
        Single ->
            SingleRadio

        CronType _ ->
            CronRadio

        Slide _ ->
            SlideRadio


radioTypeToTaskType : Date -> RadioType -> TaskType
radioTypeToTaskType defaultEndDate radioType =
    case radioType of
        SingleRadio ->
            Single

        CronRadio ->
            CronType { cron = defaultCron, cronEditValue = cronToString defaultCron, endDate = initDateModel defaultEndDate, cases = [] }

        SlideRadio ->
            Slide (initDateModel defaultEndDate)


inputTaskTypeView : TaskType -> Element Msg
inputTaskTypeView taskType =
    let
        selected =
            taskTypeToRadioType taskType
    in
    Input.radioRow [ spacing 10 ]
        { onChange = EditTaskType
        , options =
            [ Input.option SingleRadio (text "Single")
            , Input.option CronRadio (text "Cron")
            , Input.option SlideRadio (text "Slide")
            ]
        , selected = Just selected
        , label = Input.labelHidden "inputTaskTypeView"
        }


inputCronView : String -> Element Msg
inputCronView value =
    Input.text [ focused [] ]
        { onChange = EditCronValue
        , text = value
        , placeholder = Nothing
        , label = labelHidden "editCronValue"
        }


modalFooter : Bool -> Element Msg
modalFooter incorrectState =
    row [ width fill, spacing 10 ] [ cancelButton, saveButton incorrectState ]


saveButton : Bool -> Element Msg
saveButton disabled =
    if disabled then
        button
            [ Background.color gray
            , alignRight
            , padding 10
            , Border.rounded 5
            , Font.color white
            , focused []
            ]
            { onPress = Just NoOp
            , label = text "Save"
            }

    else
        button
            [ Background.color blue
            , alignRight
            , padding 10
            , Border.rounded 5
            , Font.color white
            , focused
                [ Background.color blueFocused ]
            ]
            { onPress = Just SaveTask
            , label = text "Save"
            }


cancelButton : Element Msg
cancelButton =
    button
        [ Background.color gray
        , padding 10
        , Border.rounded 5
        , alignRight
        , Font.color white
        , focused
            [ Background.color grayFocused ]
        ]
        { onPress = Just CancelEdit
        , label = text "Cancel"
        }
