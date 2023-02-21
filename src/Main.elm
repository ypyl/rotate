module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Colors exposing (black, blue, blueFocused, gray, grayColorbackground, grayFocused, white)
import Config exposing (dayTitleSize, taskSize, weekDayWidth)
import Cron exposing (Cron)
import Date exposing (Date, Unit(..), add, format, today)
import DatePicker exposing (ChangeEvent(..))
import Derberos.Date.Core exposing (civilToPosix, newDateRecord, posixToCivil)
import Derberos.Date.Delta exposing (addDays)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, focused, height, html, inFront, layout, padding, paddingEach, paddingXY, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onDoubleClick)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Mappers exposing (posixToDateStr, toPosix, toStrPosix)
import Task
import Time exposing (Month(..), now)
import TypedSvg exposing (circle, line, path, svg)
import TypedSvg.Attributes exposing (cx, cy, d, r, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, x1, x2, y1, y2)
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
    , pickerModel : DatePicker.Model
    }


type EditTask
    = EditTask TaskValue TaskValue


type alias TaskValue =
    { value : String
    , createdDate : Date
    , editDate : Date
    , date : Date
    , status : TaskStatus
    , taskType : TaskType
    }


type TaskType
    = Single
    | CronType Cron
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
            [ { value = "first", date = initialDateValue, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
            , { value = "second", date = initialDateValue, createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Slide }
            , { value = "third", date = initialDateValue, createdDate = initialDateValue, editDate = initialDateValue, status = Done, taskType = Single }
            ]
      , editTask = Nothing
      }
    , Task.perform GetToday today
    )


editTaskSample =
    let
        t =
            { value = "second"
            , date = initialDateValue
            , createdDate = initialDateValue
            , editDate = initialDateValue
            , status = Active
            , taskType = Single
            }
    in
    EditTask t t


initialStartDate =
    { date = initialDateValue
    , dateText = ""
    , pickerModel = DatePicker.init
    }


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2023 Feb 26


type Msg
    = Name String
    | SetWindowWidthHeight Int Int
    | GetToday Date
    | ChangeStartDate ChangeEvent
    | SaveTask
    | CancelEdit
    | EditValue String
    | EditTaskMsg TaskValue
    | EditTaskType TaskType
    | EditCronValue String
    | OpenStartDateDialog


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
                        , pickerModel =
                            startDateModel.pickerModel
                                |> DatePicker.setToday date
                    }
            in
            ( { model | today = date, startDate = updatedStartDate }
            , Cmd.none
            )

        ChangeStartDate changeEvent ->
            let
                startDateModel =
                    model.startDate
            in
            case changeEvent of
                DateChanged date ->
                    let
                        updatedStartDate =
                            { startDateModel
                                | date = date
                                , dateText = Date.toIsoString date
                            }
                    in
                    ( { model | startDate = updatedStartDate }
                    , Cmd.none
                    )

                TextChanged text ->
                    let
                        updatedStartDate =
                            { startDateModel
                                | date =
                                    -- parse the text in any way you like
                                    Date.fromIsoString text
                                        |> Result.toMaybe
                                        |> Maybe.withDefault model.today
                                , dateText = text
                            }
                    in
                    ( { model | startDate = updatedStartDate }
                    , Cmd.none
                    )

                PickerChanged subMsg ->
                    let
                        updatedStartDate =
                            { startDateModel
                                | pickerModel =
                                    startDateModel.pickerModel
                                        |> DatePicker.update subMsg
                            }
                    in
                    ( { model | startDate = updatedStartDate }
                    , Cmd.none
                    )

        SaveTask ->
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

        EditTaskType taskType ->
            case model.editTask of
                Just (EditTask originalTask t) ->
                    let
                        updatedTask =
                            { t | taskType = taskType }
                    in
                    ( { model | editTask = Just (EditTask originalTask updatedTask) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditCronValue cronValue ->
            ( model, Cmd.none )

        OpenStartDateDialog ->
            let
                startDateModel =
                    model.startDate

                updatedStartDate =
                    { startDateModel
                        | pickerModel =
                            startDateModel.pickerModel
                                |> DatePicker.open
                    }
            in
            ( { model | startDate = updatedStartDate }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w h -> SetWindowWidthHeight w h)



-- UI ELEMENTS


dayTitle : String -> Element msg
dayTitle value =
    el [ Font.bold, height (px dayTitleSize), centerX ] (text value)


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
    { value = "", status = Active, date = date, createdDate = date, editDate = date, taskType = Single }


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
                row [ width fill, spacing 5 ]
                    [ dayInput model.startDate
                    , dayTitle (format "E, d MMM y" weekDayDate)
                    ]

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
            task.date == date
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


dayInput : DateModel -> Element Msg
dayInput model =
    row [ alignTop ]
        [ calendarIcon
        , DatePicker.input [ width (px 0), height (px 0), alignTop, Border.width 0, focused [] ]
            { onChange = ChangeStartDate
            , selected = Just model.date
            , text = model.dateText
            , label = Input.labelHidden "dayInput"
            , placeholder = Nothing
            , settings = settings
            , model = model.pickerModel
            }
        ]


calendarIcon : Element Msg
calendarIcon =
    el [ height (px 24), width (px 24), alignTop, onClick OpenStartDateDialog ]
        (svg
            [ style "vertical-align" "middle"
            , TypedSvg.Attributes.width <| Px 24
            , TypedSvg.Attributes.height <| Px 24
            , viewBox 0 0 24 24
            , strokeWidth <| Px 1
            , TypedSvg.Attributes.fill PaintNone
            , strokeLinecap StrokeLinecapRound
            , strokeLinejoin StrokeLinejoinRound
            ]
            [ path [ stroke PaintNone, d "M0 0h24v24H0z", TypedSvg.Attributes.fill PaintNone ] []
            , path [ d "M4 5m0 2a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2z" ] []
            , path [ d "M16 3l0 4" ] []
            , path [ d "M8 3l0 4" ] []
            , path [ d "M4 11l16 0" ] []
            , path [ d "M8 15h2v2h-2z" ] []
            ]
            |> Element.html
        )


settings : DatePicker.Settings
settings =
    let
        default =
            DatePicker.defaultSettings
    in
    { default
        | pickerAttributes =
            [ Background.color white
            , padding 10
            , Border.color black
            , Border.width 1
            , Border.rounded 5
            ]
    }


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
        [ inputValueView taskValue.value
        , inputTaskView taskValue.taskType
        , modalFooter
        ]


inputValueView : String -> Element Msg
inputValueView value =
    Input.text []
        { onChange = EditValue
        , text = value
        , placeholder = Nothing
        , label = labelHidden "editValue"
        }


defaultCron : Cron
defaultCron =
    Cron.Cron Cron.Every Cron.Every Cron.Every Cron.Every Cron.Every


inputTaskView : TaskType -> Element Msg
inputTaskView taskType =
    Input.radioRow [ spacing 10 ]
        { onChange = EditTaskType
        , options =
            [ Input.option Single (text "Single")
            , Input.option (CronType defaultCron) (text "Cron")
            , Input.option Slide (text "Slide")
            ]
        , selected = Just taskType
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


modalFooter : Element Msg
modalFooter =
    row [ width fill, spacing 10 ] [ cancelButton, saveButton ]


saveButton : Element Msg
saveButton =
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
