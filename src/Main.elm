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
import Json.Decode exposing (errorToString)
import Mappers exposing (cronToString, isCronMatchDate, mapCronTaskStatusToTaskStatus, mapCronTaskToSingle, mapCronTaskToSlide, mapSingleTaskStatusToTaskStatus, mapSingleTaskToCron, mapSingleTaskToSlide, mapSlideTaskStatusToTaskStatus, mapSlideTaskToCron, mapSlideTaskToSingle, mapTaskStatusToSingleTaskStatus, mapTaskStatusToSlideTaskStatus)
import Model exposing (CronTask, CronTaskStatus(..), DatePickerType(..), EditTask(..), Model, SingleTask, SingleTaskStatus(..), SlideTaskStatus(..), TaskStatus(..), TaskValue(..), getErrors, getStartDate, getValue)
import Sync exposing (encodeModel, modelDecoder, setState)
import Task
import Time exposing (Month(..))
import TypedSvg exposing (path, svg)
import TypedSvg.Attributes as TSA
import TypedSvg.Types exposing (Length(..), Paint(..), StrokeLinecap(..), StrokeLinejoin(..))


main : Program Json.Decode.Value Model Msg
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
                    onClick CloseDatePicker :: modalWindow model

                _ ->
                    modalWindow model
    in
    layout attributesToAdd (daysView model)


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flagValue =
    let
        decodedValue =
            Json.Decode.decodeValue modelDecoder flagValue
    in
    case decodedValue of
        Ok parsedModel ->
            ( parsedModel, Task.perform GetToday today )

        Err err ->
            -- let
            --     _ =
            --         Debug.log "err" (errorToString err)
            -- in
            ( { windowWidth = 800
              , windowHeight = 600
              , startDate = initialStartDate 0
              , today = initialDateValue
              , tasks = []
              , editTask = Nothing
              , dt = Nothing
              }
            , Task.perform GetToday today
            )


initialStartDate : Int -> Date
initialStartDate delta =
    add Days delta initialDateValue


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2023 Mar 12


type Msg
    = SetWindowWidthHeight Int Int
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
                                                    case task of
                                                        CronType cronTask ->
                                                            CronType { cronTask | startDate = newDate, error = ( isThereStartDateAfterEndDate newDate cronTask.endDate, cronTask.error |> Tuple.second ) }

                                                        Slide slideTask ->
                                                            Slide { slideTask | startDate = newDate, error = isThereStartDateAfterEndDate newDate slideTask.endDate }

                                                        Single singleTasks ->
                                                            Single { singleTasks | date = newDate }
                                            in
                                            ( { model | editTask = Just (EditTask taskDate originalTask updatedTask), dt = Nothing }
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( model, Cmd.none )

                                EditEndDate ->
                                    case model.editTask of
                                        Just (EditTask taskDate originalTask task) ->
                                            case task of
                                                CronType cronTask ->
                                                    let
                                                        updatedTask =
                                                            CronType { cronTask | endDate = newDate, error = ( isThereStartDateAfterEndDate cronTask.startDate newDate, cronTask.error |> Tuple.second ) }
                                                    in
                                                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTask), dt = Nothing }
                                                    , Cmd.none
                                                    )

                                                Slide slideTask ->
                                                    let
                                                        updatedTask =
                                                            Slide { slideTask | endDate = newDate, error = isThereStartDateAfterEndDate slideTask.startDate newDate }
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
                    let
                        renewEditDate =
                            case updatedTask of
                                CronType cronTask ->
                                    CronType { cronTask | editDate = model.today }

                                Slide slideTask ->
                                    Slide { slideTask | editDate = model.today }

                                Single singleTask ->
                                    Single { singleTask | editDate = model.today }

                        updatedModel = { model | tasks = replaceTask originalTask renewEditDate model.tasks, editTask = Nothing }
                    in
                    ( updatedModel
                    , updatedModel |> encodeModel |> setState
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteTask ->
            case model.editTask of
                Just (EditTask _ originalTask _) ->
                    let
                        updatedModel = { model | tasks = deleteTask originalTask model.tasks, editTask = Nothing }
                    in
                    ( updatedModel
                    , updatedModel |> encodeModel |> setState )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editTask = Nothing }, Cmd.none )

        EditValue newValue ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
                    let
                        updatedTaskValue =
                            case task of
                                CronType cronTask ->
                                    CronType { cronTask | value = newValue }

                                Slide slideTask ->
                                    Slide { slideTask | value = newValue }

                                Single singleTask ->
                                    Single { singleTask | value = newValue }
                    in
                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTaskValue) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskMsg taskDate task ->
            ( { model | editTask = Just (EditTask taskDate task task) }, Cmd.none )

        EditTaskType radioType ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
                    ( { model | editTask = Just (EditTask taskDate originalTask (radioTypeToTaskType task radioType)) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditCronValue cronValue ->
            case model.editTask of
                Just (EditTask taskDate originalTask (CronType cronTask)) ->
                    let
                        incorrectCronError =
                            "Incorrect cron value"

                        newValue =
                            "* * " ++ cronValue

                        updatedTask =
                            case fromString newValue of
                                Ok newCron ->
                                    CronType { cronTask | cron = newCron, cronEditValue = cronToString newCron, error = ( cronTask.error |> Tuple.first, Nothing ) }

                                Err _ ->
                                    CronType { cronTask | cronEditValue = cronValue, error = ( cronTask.error |> Tuple.first, Just incorrectCronError ) }
                    in
                    ( { model | editTask = Just (EditTask taskDate originalTask updatedTask) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditTaskStatus newStatus ->
            case model.editTask of
                Just (EditTask taskDate originalTask task) ->
                    case task of
                        Single singleTask ->
                            ( { model | editTask = Just (EditTask taskDate originalTask (Single { singleTask | status = mapTaskStatusToSingleTaskStatus newStatus })) }, Cmd.none )

                        Slide slideTask ->
                            ( { model
                                | editTask = Just (EditTask taskDate originalTask (Slide { slideTask | status = mapTaskStatusToSlideTaskStatus newStatus taskDate }))
                              }
                            , Cmd.none
                            )

                        CronType cronTask ->
                            case newStatus of
                                Done ->
                                    let
                                        updatedCases =
                                            { status = CronDone, date = taskDate } :: (cronTask.cases |> List.filter (\i -> i.date /= taskDate))
                                    in
                                    ( { model | editTask = Just (EditTask taskDate originalTask (CronType { cronTask | cases = updatedCases })) }, Cmd.none )

                                Cancel ->
                                    let
                                        updatedCases =
                                            { status = CronCancel, date = taskDate } :: (cronTask.cases |> List.filter (\i -> i.date /= taskDate))
                                    in
                                    ( { model | editTask = Just (EditTask taskDate originalTask (CronType { cronTask | cases = updatedCases })) }, Cmd.none )

                                Active ->
                                    let
                                        updatedCases =
                                            cronTask.cases |> List.filter (\i -> i.date /= taskDate)
                                    in
                                    ( { model | editTask = Just (EditTask taskDate originalTask (CronType { cronTask | cases = updatedCases })) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


isThereStartDateAfterEndDate : Date -> Date -> Maybe String
isThereStartDateAfterEndDate start end =
    case Date.compare start end of
        GT ->
            Just "Start date after end date"

        _ ->
            Nothing


replaceTask : TaskValue -> TaskValue -> List TaskValue -> List TaskValue
replaceTask toReplace newTask list =
    let
        check taskValue ( result, found ) =
            if taskValue == toReplace then
                ( newTask :: result, True )

            else
                ( taskValue :: result, found )

        ( proceed, replaced ) =
            List.foldl check ( [], False ) list
    in
    if replaced then
        proceed |> List.reverse

    else
        newTask :: (proceed |> List.reverse)


deleteTask : TaskValue -> List TaskValue -> List TaskValue
deleteTask toDelete list =
    List.filter (\i -> i /= toDelete) list


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize (\w h -> SetWindowWidthHeight w h) ]


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
        ((case task of
            Single t ->
                t.value

            Slide t ->
                t.value

            CronType t ->
                t.value
         )
            |> text
        )


emptyTaskValue : Date -> ( Date, TaskValue )
emptyTaskValue date =
    ( date, SingleTask "" date date date SingleActive |> Single )


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

        cronExtraAttr : CronTask -> List (Element.Attribute Msg)
        cronExtraAttr cronTask =
            let
                caseForWeekDate =
                    cronTask.cases |> List.filter (\c -> c.date == weekDayDate) |> List.head
            in
            case caseForWeekDate of
                Just currentCase ->
                    case currentCase.status of
                        CronDone ->
                            [ Font.strike ]

                        CronCancel ->
                            [ Font.strike, Font.italic ]

                Nothing ->
                    if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                        [ Font.color red ]

                    else
                        []

        extraAttr : TaskValue -> List (Element.Attribute Msg)
        extraAttr taskValue =
            case taskValue of
                CronType cronTask ->
                    cronExtraAttr cronTask

                Single singleTask ->
                    case singleTask.status of
                        SingleActive ->
                            if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                                [ Font.color red ]

                            else
                                []

                        SingleCancel ->
                            [ Font.strike, Font.italic ]

                        SingleDone ->
                            [ Font.strike ]

                Slide slideTask ->
                    case slideTask.status of
                        SlideActive ->
                            if Date.min model.today weekDayDate == weekDayDate && weekDayDate /= model.today then
                                [ Font.color red ]

                            else
                                []

                        SlideDone _ ->
                            [ Font.strike ]

                        SlideCancel _ ->
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
    case taskValue of
        CronType cronTask ->
            isBetween cronTask.startDate cronTask.endDate date && isCronMatchDate cronTask.cron date

        Slide slideValue ->
            case slideValue.status of
                SlideActive ->
                    Date.isBetween slideValue.startDate slideValue.endDate date && Date.min today slideValue.endDate == date

                SlideDone doneDate ->
                    doneDate == date

                SlideCancel cancelDate ->
                    cancelDate == date

        Single singleTask ->
            singleTask.date == date


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
        |> el [ datePickerView |> el [ onMouseLeave (MouseInDatePicker False), onMouseEnter (MouseInDatePicker True) ] |> below ]


modalWindow : Model -> List (Element.Attribute Msg)
modalWindow model =
    case model.editTask of
        Just (EditTask editDate originalTask taskValue) ->
            let
                isNewTask =
                    (emptyTaskValue (getStartDate originalTask) |> Tuple.second) == originalTask

                alreadyPassed =
                    case taskValue of
                        Slide slideTask ->
                            if Date.min model.today slideTask.endDate == slideTask.endDate && model.today /= slideTask.endDate then
                                True

                            else
                                False

                        Single singleTask ->
                            Date.min model.today singleTask.date == singleTask.date && model.today /= singleTask.date

                        CronType cronTask ->
                            let
                                caseForEditDate =
                                    cronTask.cases |> List.filter (\c -> c.date == editDate) |> List.head
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

        startDate =
            getStartDate taskValue

        errors =
            getErrors taskValue
    in
    column [ width fill, padding 10, spacing 10 ]
        [ row twoRowAttr [ inputTaskStatusView alreadyPassed editDate taskValue, el [ alignRight ] (inputDateView startDate EditStartDate dt) ]
        , row twoRowAttr [ inputValueView (getValue taskValue), el [ alignRight ] (endDateView taskValue EditEndDate dt) ]
        , row twoRowAttr [ inputTaskTypeView taskValue ]
        , case taskValue of
            CronType cronTask ->
                if List.isEmpty errors then
                    row twoRowAttr [ inputCronView cronTask.cronEditValue, cronView (toString cronTask.cron) ]

                else
                    row twoRowAttr [ inputCronView cronTask.cronEditValue, errorView errors ]

            _ ->
                if List.isEmpty errors then
                    none

                else
                    row twoRowAttr [ none, errorView errors ]
        , modalFooter isNewTask (List.isEmpty errors |> not)
        ]


inputTaskStatusView : Bool -> Date -> TaskValue -> Element Msg
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
                            foundCase.status |> mapCronTaskStatusToTaskStatus

                        Nothing ->
                            Active

                Slide slideTask ->
                    slideTask.status |> mapSlideTaskStatusToTaskStatus

                Single singleTask ->
                    singleTask.status |> mapSingleTaskStatusToTaskStatus
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
    case taskValue of
        Single _ ->
            none

        CronType cronTask ->
            inputEndView cronTask.endDate datePickerType dt

        Slide slideTask ->
            inputEndView slideTask.endDate datePickerType dt


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


type RadioType
    = SingleRadio
    | CronRadio
    | SlideRadio


taskTypeToRadioType : TaskValue -> RadioType
taskTypeToRadioType taskValue =
    case taskValue of
        Single _ ->
            SingleRadio

        CronType _ ->
            CronRadio

        Slide _ ->
            SlideRadio


radioTypeToTaskType : TaskValue -> RadioType -> TaskValue
radioTypeToTaskType currentTask radioType =
    case ( radioType, currentTask ) of
        ( SingleRadio, Slide slideTask ) ->
            mapSlideTaskToSingle slideTask |> Single

        ( SingleRadio, Single _ ) ->
            currentTask

        ( SingleRadio, CronType cronTask ) ->
            mapCronTaskToSingle cronTask |> Single

        ( CronRadio, Single singleTask ) ->
            mapSingleTaskToCron singleTask |> CronType

        ( CronRadio, Slide slideValue ) ->
            mapSlideTaskToCron slideValue |> CronType

        ( CronRadio, CronType _ ) ->
            currentTask

        ( SlideRadio, Single singleTask ) ->
            mapSingleTaskToSlide singleTask |> Slide

        ( SlideRadio, Slide _ ) ->
            currentTask

        ( SlideRadio, CronType cronValue ) ->
            mapCronTaskToSlide cronValue |> Slide


inputTaskTypeView : TaskValue -> Element Msg
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
