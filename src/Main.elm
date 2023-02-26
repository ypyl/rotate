module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Colors exposing (black, blue, blueFocused, gray, grayColorbackground, grayFocused, white)
import Config exposing (dayTitleSize, taskSize, weekDayWidth)
import Cron exposing (Cron, fromString)
import Date exposing (Date, Unit(..), add, format, fromIsoString, today)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, focused, height, htmlAttribute, inFront, layout, none, padding, paddingXY, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onDoubleClick)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Html exposing (Html)
import Html.Attributes exposing (class, style, type_)
import Humanizer exposing (toString)
import Mappers exposing (cronToString)
import Parser exposing (deadEndsToString)
import Task
import Time exposing (Month(..))
import TypedSvg exposing (path, svg)
import TypedSvg.Attributes exposing (d, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)
import TypedSvg.Types exposing (Length(..), Paint(..), StrokeLinecap(..), StrokeLinejoin(..))
import Date exposing (toIsoString)
import Element exposing (shrink)


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
    , error : Maybe String
    }


type TaskType
    = Single
    | CronType Cron String
    | Slide


type TaskStatus
    = Done
    | Active
    | Cancel
    | Fail


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( windowWidth, windowHeight ) =
    ( { message = "hey"
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startDate = initialStartDate
      , today = initialDateValue
      , tasks =
            [ { value = "first", date = initialStartDate, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single, error = Nothing }
            , { value = "second", date = initialStartDate, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Slide, error = Nothing }
            , { value = "third", date = initialStartDate, createdDate = initialDateValue, editDate = initialDateValue, status = Done, taskType = Single, error = Nothing }
            ]
      , editTask = Nothing
      }
    , Task.perform GetToday today
    )


initialStartDate : { date : Date, dateText : String }
initialStartDate =
    { date = initialDateValue
    , dateText = toIsoString initialDateValue
    }


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2023 Feb 28


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
                            { t | taskType = radioTypeToTaskType radioType }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditCronValue cronValue ->
            case model.editTask of
                Just (EditTask originalTask t) ->
                    let
                        newValue =
                            "* * " ++ cronValue

                        updatedTask =
                            case ( fromString newValue, t.taskType ) of
                                ( Ok newCron, _ ) ->
                                    { t | taskType = CronType newCron (cronToString newCron), error = Nothing }

                                ( Err errorValue, CronType currentValue _ ) ->
                                    { t | error = Just (deadEndsToString errorValue), taskType = CronType currentValue cronValue }

                                ( Err errorValue, _ ) ->
                                    { t | error = Just (deadEndsToString errorValue) }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskDate newDate ->
            case (model.editTask, fromIsoString newDate) of
                (Just (EditTask originalTask t), Ok newParsedDate) ->
                    let
                        updatedTask = { t | date = updatedDateModel t.date }
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

        NoOp ->
            ( model, Cmd.none )


replaceTask : TaskValue -> TaskValue -> List TaskValue -> List TaskValue
replaceTask toReplace newTask list =
    let
        check taskValue result =
            if taskValue == toReplace then
                newTask :: result

            else
                taskValue :: result
    in
    List.foldl check [] list |> List.reverse


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w h -> SetWindowWidthHeight w h)



-- UI ELEMENTS


dayTitle : String -> Element msg
dayTitle value =
    el [ paddingXY 0 5, height (px dayTitleSize), centerX ] (text value)


taskValueView : TaskValue -> Element Msg
taskValueView task =
    let
        extraAttr =
            case task.status of
                Done ->
                    [ Font.strike ]

                Active ->
                    []

                Cancel ->
                    [ Font.strike ]

                Fail ->
                    [ Font.light ]
    in
    el
        ([ width fill
         , height (px taskSize)
         , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
         , Border.color (rgb255 0 0 0)
         , paddingXY 3 0
         , onDoubleClick (EditTaskMsg task)
         , Html.Attributes.class "no-select" |> Element.htmlAttribute
         ]
            ++ extraAttr
        )
        (text task.value)


emptyTaskValue : Date -> TaskValue
emptyTaskValue date =
    let
        dateModel =
            { date = date
            , dateText = ""
            }
    in
    { value = "", status = Active, date = dateModel, createdDate = date, editDate = date, taskType = Single, error = Nothing }


weekDay : Model -> Int -> Element Msg
weekDay model dayDelta =
    let
        weekDayDate =
            add Days dayDelta model.startDate.date

        weekDayTasks =
            filteredTaskPerDay weekDayDate model.tasks

        emptyTasks count =
            List.repeat count (taskValueView (emptyTaskValue weekDayDate))

        totalCountOfTasksToShow =
            numberOfTaskToShow model.windowHeight

        emptyTaskCount =
            if totalCountOfTasksToShow - List.length weekDayTasks > 0 then
                totalCountOfTasksToShow - List.length weekDayTasks

            else
                0

        title =
            if dayDelta == 0 then
                dayInput model.startDate ChangeStartDate

            else
                dayTitle (format "E, d MMM y" weekDayDate)

        viewTask task =
            taskValueView task
    in
    column
        [ Font.color black
        , Border.color black

        --, Border.width 1
        , Border.rounded 3
        , padding 3
        , width (px weekDayWidth)
        ]
        (title :: List.map viewTask weekDayTasks ++ emptyTasks emptyTaskCount)



-- TODO use dict to find tasks for days (there will be a special case for different types)


filteredTaskPerDay : Date -> List TaskValue -> List TaskValue
filteredTaskPerDay date tasks =
    let
        filterFunc task =
            task.date.date == date
    in
    List.filter filterFunc tasks


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
        [ Input.text [ type_ "date" |> htmlAttribute, class "date-input" |> htmlAttribute, Border.width 0, focused [], width (px 170) ]
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
    column [ width fill, padding 10, spacing 10 ]
        [ row [ width fill ] [ inputValueView taskValue.value , inputDateView taskValue.date ]
        , inputTaskView taskValue.taskType
        , case taskValue.taskType of
            CronType _ editValue ->
                inputCronView editValue

            _ ->
                none
        , case ( taskValue.error, taskValue.taskType ) of
            ( Just _, CronType _ _ ) ->
                cronView "Incorrect cron string"

            ( Nothing, CronType cronValue _ ) ->
                cronView (toString cronValue)

            _ ->
                none
        , case ( taskValue.error, taskValue.taskType ) of
            ( Just _, _ ) ->
                modalFooter True

            _ ->
                modalFooter False
        ]


cronView : String -> Element msg
cronView strCron =
    let
        result =
            case String.split "," strCron of
                _ :: _ :: rest ->
                    String.join "," rest

                _ ->
                    strCron
    in
    paragraph [ Font.size 14 ] [ text result ]


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

        CronType _ _ ->
            CronRadio

        Slide ->
            SlideRadio


radioTypeToTaskType : RadioType -> TaskType
radioTypeToTaskType radioType =
    case radioType of
        SingleRadio ->
            Single

        CronRadio ->
            CronType defaultCron (cronToString defaultCron)

        SlideRadio ->
            Slide


inputTaskView : TaskType -> Element Msg
inputTaskView taskType =
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
        , label = Input.labelHidden "inputTaskView"
        }


inputCronView : String -> Element Msg
inputCronView value =
    Input.text []
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
            , focused
                [ Background.color blueFocused ]
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
