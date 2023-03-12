module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Colors exposing (black, blue, gray, grayColorbackground, red, white)
import Config exposing (dayTitleSize, modalSecondColumnWidth, taskSize, weekDayWidth)
import Cron exposing (Cron, WeekDay(..), fromString)
import Date exposing (Date, Unit(..), add, format, isBetween, today)
import DatePicker as DT
import Element exposing (Element, alignLeft, alignRight, below, centerX, centerY, column, el, fill, focused, height, html, inFront, layout, map, none, padding, paddingEach, paddingXY, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onDoubleClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Humanizer exposing (toString)
import Mappers exposing (cronToString, isCronMatchDate)
import Task
import Time exposing (Month(..))
import TypedSvg exposing (path, svg)
import TypedSvg.Attributes as TSA
import TypedSvg.Types exposing (Length(..), Paint(..), StrokeLinecap(..), StrokeLinejoin(..))


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
        attributesToAdd =
            case model.dt of
                Just ( _, _, False ) ->
                    (onClick CloseDatePicker :: modalWindow model)
                _ -> modalWindow model
    in
    layout attributesToAdd (daysView model)


type alias Model =
    { message : String
    , windowWidth : Int
    , windowHeight : Int
    , today : Date
    , startDate : Date
    , tasks : List TaskValue
    , editTask : Maybe EditTask
    , dt : Maybe ( DatePickerType, DT.Model, Bool )
    }


type DatePickerType
    = StartDate
    | EditStartDate
    | EditEndDate


type EditTask
    = EditTask Date TaskValue TaskValue


type alias TaskValue =
    { value : String
    , createdDate : Date
    , editDate : Date
    , date : Date
    , taskType : TaskType
    , error : List String
    }


type TaskType
    = Single TaskStatus
    | CronType CronTaskValue
    | Slide SlideTaskValue


type alias SlideTaskValue =
    { endDate : Date
    , status : TaskStatus
    }


type alias CronTaskValue =
    { cron : Cron
    , cronEditValue : String
    , endDate : Date
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
      , tasks = []
            -- [ { value = "single-active", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Single Active, error = [] }
            -- , { value = "slide-active", date = initialStartDate -5, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue 10 Active), error = [] }
            -- , { value = "slide-failed", date = initialStartDate -5, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue -1 Active), error = [] }
            -- , { value = "cron-active", date = initialStartDate -100, createdDate = initialDateValue, editDate = initialDateValue, taskType = CronType initialCronTaskValue, error = [] }
            -- , { value = "single-done", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Single Done, error = [] }
            -- , { value = "slide-done", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue 10 Done), error = [] }
            -- , { value = "single-cancel", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Single Cancel, error = [] }
            -- , { value = "slide-cancel", date = initialStartDate 0, createdDate = initialDateValue, editDate = initialDateValue, taskType = Slide (initialSlideTaskValue 10 Cancel), error = [] }
            -- ]
      , editTask = Nothing
      , dt = Nothing
      }
    , Task.perform GetToday today
    )


initialSlideTaskValue : Int -> TaskStatus -> { endDate : Date, status : TaskStatus }
initialSlideTaskValue delta status =
    { endDate = initialStartDate delta, status = status }


initialCronTaskValue : { cron : Cron, cronEditValue : String, endDate : Date, cases : List a }
initialCronTaskValue =
    { cron = testCron, cronEditValue = cronToString testCron, endDate = initialStartDate 100, cases = [] }


testCron : Cron
testCron =
    Cron.Cron Cron.Every Cron.Every Cron.Every Cron.Every (Cron.Single (Cron.Atom (Cron.Range Monday Friday)))


initialStartDate : Int -> Date
initialStartDate delta =
    add Days delta initialDateValue


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2023 Mar 12


type Msg
    = Name String
    | SetWindowWidthHeight Int Int
    | GetToday Date
    | SaveTask
    | CancelEdit
    | EditValue String
    | EditTaskMsg Date TaskValue
    | EditTaskType RadioType
    | EditCronValue String
    | EditTaskStatus TaskStatus
    | DeleteTask
    | DT DT.Msg
    | ShowDatePicker Date DatePickerType
    | CloseDatePicker
    | MouseInDatePicker Bool
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DT subMsg ->
            case model.dt of
                Just ( datePickerType, dtModel, mouseOnDT ) ->
                    let
                        updatedDtModel =
                            DT.update subMsg dtModel

                        datePickerSelectedDate =
                            DT.getLastSelectedDate updatedDtModel
                    in
                    case datePickerSelectedDate of
                        Just newDate ->
                            case datePickerType of
                                StartDate ->
                                    ( { model | dt = Nothing, startDate = newDate }, Cmd.none )

                                EditStartDate ->
                                    case model.editTask of
                                        Just (EditTask taskDate originalTask task) ->
                                            let
                                                updatedTask =
                                                    case task.taskType of
                                                        CronType cronValue ->
                                                            { task | date = newDate, error = isThereStartDateAfterEndDate newDate cronValue.endDate task.error }

                                                        Slide slideValue ->
                                                            { task | date = newDate, error = isThereStartDateAfterEndDate newDate slideValue.endDate task.error }

                                                        Single _ ->
                                                            { task | date = newDate }
                                            in
                                            ( { model | editTask = Just (EditTask taskDate originalTask updatedTask), dt = Nothing }
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( model, Cmd.none )

                                EditEndDate ->
                                    case model.editTask of
                                        Just (EditTask taskDate originalTask task) ->
                                            case task.taskType of
                                                CronType cronValue ->
                                                    let
                                                        updatedCronValue =
                                                            { cronValue | endDate = newDate }

                                                        updatedTask =
                                                            { task | taskType = CronType updatedCronValue, error = isThereStartDateAfterEndDate task.date newDate task.error }
                                                    in
                                                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTask), dt = Nothing }
                                                    , Cmd.none
                                                    )

                                                Slide slideValue ->
                                                    let
                                                        updatedTask =
                                                            { task | taskType = Slide { slideValue | endDate = newDate }, error = isThereStartDateAfterEndDate task.date newDate task.error }
                                                    in
                                                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTask), dt = Nothing }
                                                    , Cmd.none
                                                    )

                                                _ ->
                                                    ( model, Cmd.none )

                                        _ ->
                                            ( model, Cmd.none )

                        Nothing ->
                            ( { model | dt = Just ( datePickerType, updatedDtModel, mouseOnDT ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ShowDatePicker date datePickerType ->
            ( { model | dt = Just ( datePickerType, DT.create date, True ) }, Cmd.none )

        MouseInDatePicker value ->
            case model.dt of
                Just ( datePickerType, dtModel, _ ) ->
                    ( { model | dt = Just ( datePickerType, dtModel, value ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CloseDatePicker ->
            ( { model | dt = Nothing }, Cmd.none )

        Name name ->
            ( { model | message = name }, Cmd.none )

        SetWindowWidthHeight width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

        GetToday date ->
            ( { model
                | today = date
                , startDate = date
              }
            , Cmd.none
            )

        SaveTask ->
            case model.editTask of
                Just (EditTask _ originalTask updatedTask) ->
                    ( { model | tasks = replaceTask originalTask { updatedTask | editDate = model.today } model.tasks, editTask = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeleteTask ->
            case model.editTask of
                Just (EditTask _ originalTask _) ->
                    ( { model | tasks = deleteTask originalTask model.tasks, editTask = Nothing }, Cmd.none )

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
                    ( { model | editTask = Just (EditTask taskDate originalTask { task | taskType = radioTypeToTaskType originalTask radioType }) }, Cmd.none )

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
    if (emptyTaskValue toReplace.date |> Tuple.second) == toReplace then
        newTask :: list

    else
        List.foldl check [] list |> List.reverse


deleteTask : TaskValue -> List TaskValue -> List TaskValue
deleteTask toDelete list =
    List.filter (\i -> i /= toDelete) list


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
    ( date, { value = "", date = date, createdDate = date, editDate = date, taskType = Single Active, error = [] } )


weekDay : Model -> Int -> Element Msg
weekDay model dayDelta =
    let
        atLeastOneEmptyTaskPerDay =
            1

        weekDayDate =
            add Days dayDelta model.startDate

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
                dayInput model.startDate StartDate model.dt

            else
                dayTitle (format "E, d MMM y" weekDayDate)

        cronExtraAttr : CronTaskValue -> List (Element.Attribute Msg)
        cronExtraAttr cronValue =
            let
                caseForWeekDate =
                    cronValue.cases |> List.filter (\c -> c.date == weekDayDate) |> List.head
            in
            case caseForWeekDate of
                Just currentCase ->
                    case currentCase.status of
                        Done ->
                            [ Font.strike ]

                        Cancel ->
                            [ Font.strike, Font.italic ]

                        Active ->
                            []

                Nothing ->
                    if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                        [ Font.color red ]

                    else
                        []

        extraAttr : TaskValue -> List (Element.Attribute Msg)
        extraAttr taskValue =
            case taskValue.taskType of
                CronType cronValue ->
                    cronExtraAttr cronValue

                Single Active ->
                    if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                        [ Font.color red ]

                    else
                        []

                Single Done ->
                    [ Font.strike ]

                Single Cancel ->
                    [ Font.strike, Font.italic ]

                Slide slideValue ->
                    case slideValue.status of
                        Active ->
                            if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                                [ Font.color red ]

                            else
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
            isBetween taskValue.date cronValue.endDate date && isCronMatchDate cronValue.cron date

        Slide slideValue ->
            case slideValue.status of
                Active ->
                    Date.min today slideValue.endDate == date

                _ ->
                    taskValue.date == date

        _ ->
            taskValue.date == date


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
    viewWidth // weekDayWidth


dayInput : Date -> DatePickerType -> Maybe ( DatePickerType, DT.Model, Bool ) -> Element Msg
dayInput date datePickerType dt =
    let
        datePickerView =
            case dt of
                Just ( dtType, dtModel, _ ) ->
                    if dtType == datePickerType then
                        DT.view dtModel |> map DT

                    else
                        none

                Nothing ->
                    none
    in
    button [ paddingXY 0 5, focused [] ]
        { onPress = ShowDatePicker date datePickerType |> Just
        , label =
            row []
                [ el [ paddingEach { top = 5, left = 0, right = 10, bottom = 5 }, height (px dayTitleSize), centerX ] (text (format "E, d MMM y" date))
                , svg
                    [ style "vertical-align" "middle"
                    , TSA.width <| Px 24
                    , TSA.height <| Px 24
                    , TSA.viewBox 0 0 24 24
                    , TSA.strokeWidth <| Px 1
                    , TSA.fill PaintNone
                    , TSA.strokeLinecap StrokeLinecapRound
                    , TSA.strokeLinejoin StrokeLinejoinRound
                    ]
                    [ path [ TSA.stroke PaintNone, TSA.d "M0 0h24v24H0z", TSA.fill PaintNone ] []
                    , path [ TSA.d "M4 7a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2v-12z" ] []
                    , path [ TSA.d "M16 3v4" ] []
                    , path [ TSA.d "M8 3v4" ] []
                    , path [ TSA.d "M4 11h16" ] []
                    , path [ TSA.d "M11 15h1" ] []
                    , path [ TSA.d "M12 15v3" ] []
                    ]
                    |> html
                ]
        }
        |> el [ datePickerView |> below, onMouseLeave (MouseInDatePicker False), onMouseEnter (MouseInDatePicker True) ]


modalWindow : Model -> List (Element.Attribute Msg)
modalWindow model =
    case model.editTask of
        Just (EditTask editDate originalTask taskValue) ->
            let
                isNewTask =
                    (emptyTaskValue originalTask.date |> Tuple.second) == originalTask

                alreadyPassed =
                    case taskValue.taskType of
                        Slide slideValue ->
                            if Date.min model.today slideValue.endDate == slideValue.endDate && model.today /= slideValue.endDate then
                                True

                            else
                                False

                        Single _ ->
                            Date.min model.today taskValue.date == taskValue.date && model.today /= taskValue.date

                        CronType cronValue ->
                            let
                                caseForEditDate =
                                    cronValue.cases |> List.filter (\c -> c.date == editDate) |> List.head
                            in
                            case caseForEditDate of
                                Just foundCase ->
                                    Date.min model.today foundCase.date == foundCase.date && model.today /= foundCase.date

                                Nothing ->
                                    Date.min model.today editDate == editDate && editDate /= model.today
            in
            [ editTaskView alreadyPassed isNewTask editDate taskValue model.dt
                |> el
                    [ centerX
                    , centerY
                    , Background.color white
                    , Border.rounded 5
                    ]
                |> el
                    [ width fill
                    , height fill
                    , Background.color grayColorbackground
                    ]
            ]
                |> List.map inFront

        Nothing ->
            []


editTaskView : Bool -> Bool -> Date -> TaskValue -> Maybe ( DatePickerType, DT.Model, Bool ) -> Element Msg
editTaskView alreadyPassed isNewTask editDate taskValue dt =
    let
        twoRowAttr =
            [ spacing 10, width fill ]
    in
    column [ width fill, padding 10, spacing 10 ]
        [ row twoRowAttr [ inputTaskStatusView alreadyPassed editDate taskValue.taskType, el [ alignRight ] (inputDateView taskValue.date EditStartDate dt) ]
        , row twoRowAttr [ inputValueView taskValue.value, el [ alignRight ] (endDateView taskValue EditEndDate dt) ]
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
        , modalFooter isNewTask (List.isEmpty taskValue.error |> not)
        ]


inputTaskStatusView : Bool -> Date -> TaskType -> Element Msg
inputTaskStatusView alreadyPassed editDate taskType =
    let
        selectedValue =
            case taskType of
                CronType cronValue ->
                    let
                        caseForEditDate =
                            cronValue.cases |> List.filter (\c -> c.date == editDate) |> List.head
                    in
                    case caseForEditDate of
                        Just foundCase ->
                            foundCase.status

                        Nothing ->
                            Active

                Slide slideValue ->
                    slideValue.status

                Single status ->
                    status
    in
    Input.radioRow [ spacing 10 ]
        { onChange = EditTaskStatus
        , options =
            [ Input.option Active
                (text
                    (if alreadyPassed then
                        "Failed"

                     else
                        "Active"
                    )
                )
            , Input.option Cancel (text "Cancelled")
            , Input.option Done (text "Done")
            ]
        , selected = Just selectedValue
        , label = Input.labelHidden "inputTaskStatusView"
        }


endDateView : TaskValue -> DatePickerType -> Maybe ( DatePickerType, DT.Model, Bool ) -> Element Msg
endDateView taskValue datePickerType dt =
    case taskValue.taskType of
        Single _ ->
            none

        CronType cronTaskValue ->
            inputEndView cronTaskValue.endDate datePickerType dt

        Slide slideTaskValue ->
            inputEndView slideTaskValue.endDate datePickerType dt


inputEndView : Date -> DatePickerType -> Maybe ( DatePickerType, DT.Model, Bool ) -> Element Msg
inputEndView dateValue datePickerType dt =
    dayInput dateValue datePickerType dt


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


inputDateView : Date -> DatePickerType -> Maybe ( DatePickerType, DT.Model, Bool ) -> Element Msg
inputDateView dateValue datePickerType dt =
    dayInput dateValue datePickerType dt


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
radioTypeToTaskType originalTask radioType =
    case ( radioType, originalTask.taskType ) of
        ( SingleRadio, Single status ) ->
            Single status

        ( SingleRadio, Slide slideValue ) ->
            Single slideValue.status

        ( SingleRadio, CronType _ ) ->
            Single Active

        ( CronRadio, Single _ ) ->
            CronType { cron = defaultCron, cronEditValue = cronToString defaultCron, endDate = originalTask.date, cases = [] }

        ( CronRadio, Slide slideValue ) ->
            CronType { cron = defaultCron, cronEditValue = cronToString defaultCron, endDate = slideValue.endDate, cases = [] }

        ( CronRadio, CronType cronValue ) ->
            CronType cronValue

        ( SlideRadio, Single status ) ->
            Slide { endDate = originalTask.date, status = status }

        ( SlideRadio, Slide slideValue ) ->
            Slide slideValue

        ( SlideRadio, CronType cronValue ) ->
            Slide { endDate = cronValue.endDate, status = Active }


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


modalFooter : Bool -> Bool -> Element Msg
modalFooter isNewTask incorrectState =
    row [ width fill, spacing 10 ]
        [ if isNewTask then
            none

          else
            deleteButton
        , cancelButton
        , saveButton incorrectState
        ]


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
                []
            ]
            { onPress = Just SaveTask
            , label = text "Save"
            }


deleteButton : Element Msg
deleteButton =
    button
        [ Background.color red
        , padding 10
        , Border.rounded 5
        , alignLeft
        , Font.color white
        , focused
            []
        ]
        { onPress = Just DeleteTask
        , label = text "Delete"
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
            []
        ]
        { onPress = Just CancelEdit
        , label = text "Cancel"
        }
