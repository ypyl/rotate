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
                Just (EditTask _ _ t) ->
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
    = EditTask Date TaskValue TaskValue


type alias TaskValue =
    { value : String
    , createdDate : Date
    , editDate : Date
    , date : DateModel
    , taskType : TaskType
    , error : List String
    }


type TaskType
    = Single TaskStatus
    | CronType CronTaskValue
    | Slide SlideTaskValue


type alias SlideTaskValue =
    { endDate : DateModel
    , status : TaskStatus
    }


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
            [ { value = "single-active", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Single Active, error = [] }
            , { value = "slide-active", date = initialStartDate -5, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue Active), error = [] }
            , { value = "cron-active", date = initialStartDate -100, createdDate = initialDateValue, editDate = initialDateValue, taskType = CronType initialCronTaskValue, error = [] }
            , { value = "single-done", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Single Done, error = [] }
            , { value = "slide-done", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue Done), error = [] }
            , { value = "single-cancel", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Single Cancel, error = [] }
            , { value = "slide-cancel", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue Cancel), error = [] }
            ]
      , editTask = Nothing
      }
    , Task.perform GetToday today
    )


initialSlideTaskValue : TaskStatus -> { endDate : { date : Date, dateText : String }, status : TaskStatus }
initialSlideTaskValue status =
    { endDate = initialStartDate 10, status = status }


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
    Date.fromCalendarDate 2023 Mar 4


type Msg
    = Name String
    | SetWindowWidthHeight Int Int
    | GetToday Date
    | ChangeStartDate String
    | EditTaskDate String
    | SaveTask
    | CancelEdit
    | EditValue String
    | EditTaskMsg Date TaskValue
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
            in
            ( { model
                | today = date
                , startDate =
                    { startDateModel
                        | date = date
                        , dateText = Date.toIsoString date
                    }
              }
            , Cmd.none
            )

        ChangeStartDate newDate ->
            case fromIsoString newDate of
                Ok newDateParser ->
                    let
                        startDateModel =
                            model.startDate
                    in
                    ( { model
                        | startDate =
                            { startDateModel
                                | date = newDateParser
                                , dateText = newDate
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SaveTask ->
            case model.editTask of
                Just (EditTask _ originalTask updatedTask) ->
                    ( { model | tasks = replaceTask originalTask { updatedTask | editDate = model.today } model.tasks, editTask = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editTask = Nothing }, Cmd.none )

        EditValue newValue ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
                    ( { model | editTask = Just (EditTask taskDate originalTask { task | value = newValue }) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskMsg taskDate task ->
            ( { model | editTask = Just (EditTask taskDate task task) }, Cmd.none )

        EditTaskType radioType ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
                    ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = radioTypeToTaskType task radioType }) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditCronValue cronValue ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
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
                            case ( fromString newValue, task.taskType ) of
                                ( Ok newCron, CronType cronTaskValue ) ->
                                    { task | taskType = CronType { cronTaskValue | cron = newCron, cronEditValue = cronToString newCron }, error = removeError task.error }

                                ( Ok _, _ ) ->
                                    task

                                ( Err _, CronType cronTaskValue ) ->
                                    { task | error = addError task.error, taskType = CronType { cronTaskValue | cronEditValue = cronValue } }

                                ( Err _, _ ) ->
                                    { task | error = addError task.error }
                    in
                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskDate newDate ->
            case ( model.editTask, fromIsoString newDate ) of
                ( Just (EditTask taskDate originalTask task), Ok newParsedDate ) ->
                    let
                        updatedTask =
                            case task.taskType of
                                CronType cronValue ->
                                    { task | date = updatedDateModel task.date, error = isThereStartDateAfterEndDate newParsedDate cronValue.endDate.date task.error }

                                Slide slideValue ->
                                    { task | date = updatedDateModel task.date, error = isThereStartDateAfterEndDate newParsedDate slideValue.endDate.date task.error }

                                Single _ ->
                                    { task | date = updatedDateModel task.date }

                        updatedDateModel modelToUpdate =
                            { modelToUpdate
                                | date = newParsedDate
                                , dateText = newDate
                            }
                    in
                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTask) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditTaskEndDate newDate ->
            case ( model.editTask, fromIsoString newDate ) of
                ( Just (EditTask taskDate originalTask task), Ok newParsedDate ) ->
                    case task.taskType of
                        CronType cronValue ->
                            let
                                updatedCronValue =
                                    { cronValue | endDate = updatedDateModel cronValue.endDate }

                                updatedTask =
                                    { task | taskType = CronType updatedCronValue, error = isThereStartDateAfterEndDate task.date.date newParsedDate task.error }

                                updatedDateModel modelToUpdate =
                                    { modelToUpdate
                                        | date = newParsedDate
                                        , dateText = newDate
                                    }
                            in
                            ( { model | editTask = Just (EditTask taskDate originalTask updatedTask) }
                            , Cmd.none
                            )

                        Slide slideValue ->
                            let
                                updatedTask =
                                    { task | taskType = Slide { slideValue | endDate = updatedDateModel slideValue.endDate }, error = isThereStartDateAfterEndDate task.date.date newParsedDate task.error }

                                updatedDateModel modelToUpdate =
                                    { modelToUpdate
                                        | date = newParsedDate
                                        , dateText = newDate
                                    }
                            in
                            ( { model | editTask = Just (EditTask taskDate originalTask updatedTask) }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditTaskStatus newStatus ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
                    case task.taskType of
                        Single _ ->
                            ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = Single newStatus }) }, Cmd.none )

                        Slide slideValue ->
                            ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = Slide { slideValue | status = newStatus } }) }, Cmd.none )

                        CronType cronValue ->
                            case newStatus of
                                Done ->
                                    let
                                        updatedCases =
                                            { date = taskDate, status = Done } :: (cronValue.cases |> List.filter (\i -> i.date /= taskDate))
                                    in
                                    ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = CronType { cronValue | cases = updatedCases } }) }, Cmd.none )

                                Cancel ->
                                    let
                                        updatedCases =
                                            { date = taskDate, status = Cancel } :: (cronValue.cases |> List.filter (\i -> i.date /= taskDate))
                                    in
                                    ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = CronType { cronValue | cases = updatedCases } }) }, Cmd.none )

                                Active ->
                                    let
                                        updatedCases =
                                            cronValue.cases |> List.filter (\i -> i.date /= taskDate)
                                    in
                                    ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = CronType { cronValue | cases = updatedCases } }) }, Cmd.none )

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
    if (emptyTaskValue toReplace.date.date |> Tuple.second) == toReplace then
        newTask :: list

    else
        List.foldl check [] list |> List.reverse


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w h -> SetWindowWidthHeight w h)


dayTitle : String -> Element msg
dayTitle value =
    el [ paddingXY 0 5, height (px dayTitleSize), centerX ] (text value)


taskValueView : (TaskValue -> List (Element.Attribute Msg)) -> ( Date, TaskValue ) -> Element Msg
taskValueView extraAttr ( taskDate, task ) =
    el
        ([ width fill
         , height (px taskSize)
         , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
         , Border.color (rgb255 0 0 0)
         , paddingXY 3 0
         , onDoubleClick (EditTaskMsg taskDate task)
         , Html.Attributes.class "no-select" |> Element.htmlAttribute
         ]
            ++ extraAttr task
        )
        (text task.value)


emptyTaskValue : Date -> ( Date, TaskValue )
emptyTaskValue date =
    let
        dateModel =
            { date = date
            , dateText = toIsoString date
            }
    in
    ( date, { value = "", date = dateModel, createdDate = date, editDate = date, taskType = Single Active, error = [] } )


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

        extraAttr : TaskValue -> List (Element.Attribute Msg)
        extraAttr taskValue =
            case taskValue.taskType of
                CronType _ ->
                    []

                Single Active ->
                    if Date.min taskValue.date.date weekDayDate == weekDayDate && Date.min model.today taskValue.date.date == taskValue.date.date && weekDayDate /= model.today then
                        [ Font.strike ]

                    else
                        []

                Single Done ->
                    [ Font.strike ]

                Single Cancel ->
                    [ Font.strike, Font.italic ]

                Slide slideValue ->
                    case slideValue.status of
                        Active ->
                            []

                        Done ->
                            [ Font.strike ]

                        Cancel ->
                            [ Font.strike, Font.italic ]
    in
    column
        [ Font.color black
        , Border.color black
        , padding 3
        , width (px weekDayWidth)
        ]
        (title :: List.map (taskValueView extraAttr) weekDayTasks ++ emptyTasks emptyTaskCount)


filteredTaskPerDay : Date -> Date -> List TaskValue -> List ( Date, TaskValue )
filteredTaskPerDay today date tasks =
    List.filter (showTaskAtDate today date) tasks |> List.map (Tuple.pair date)


showTaskAtDate : Date -> Date -> TaskValue -> Bool
showTaskAtDate today date taskValue =
    case taskValue.taskType of
        CronType cronValue ->
            isBetween today cronValue.endDate.date date && isCronMatchDate cronValue.cron date

        Slide slideValue ->
            Date.min today slideValue.endDate.date == date

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
        [ row twoRowAttr [ inputTaskStatusView taskValue.taskType, el [ alignRight ] (inputDateView taskValue.date) ]
        , row twoRowAttr [ inputValueView taskValue.value, el [ alignRight ] (endDateView taskValue) ]
        , row twoRowAttr [ inputTaskTypeView taskValue.taskType ]
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


inputTaskStatusView : TaskType -> Element Msg
inputTaskStatusView taskType =
    let
        selectedValue =
            case taskType of
                CronType _ ->
                    Active
                Slide slideValue ->
                    slideValue.status
                Single status ->
                    status
    in
    Input.radioRow [ spacing 10 ]
        { onChange = EditTaskStatus
        , options =
            [ Input.option Active (text "Active")
            , Input.option Cancel (text "Cancelled")
            , Input.option Done (text "Done")
            ]
        , selected = Just selectedValue
        , label = Input.labelHidden "inputTaskStatusView"
        }



endDateView : TaskValue -> Element Msg
endDateView taskValue =
    case taskValue.taskType of
        Single _ ->
            none

        CronType cronTaskValue ->
            inputEndView cronTaskValue.endDate

        Slide slideTaskValue ->
            inputEndView slideTaskValue.endDate


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
        Single _ ->
            SingleRadio

        CronType _ ->
            CronRadio

        Slide _ ->
            SlideRadio


radioTypeToTaskType : TaskValue -> RadioType -> TaskType
radioTypeToTaskType task radioType =
    case radioType of
        SingleRadio ->
            Single Active

        CronRadio ->
            CronType { cron = defaultCron, cronEditValue = cronToString defaultCron, endDate = initDateModel task.date.date, cases = [] }

        SlideRadio ->
            Slide { endDate = initDateModel task.date.date, status = Active }


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
