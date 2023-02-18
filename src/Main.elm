module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Colors exposing (black, blue, blueFocused, gray, grayColorbackground, grayFocused, white)
import Config exposing (dayTitleSize, taskSize, weekDayWidth)
import Derberos.Date.Core exposing (civilToPosix, newDateRecord, posixToCivil)
import Derberos.Date.Delta exposing (addDays)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, focused, height, html, inFront, layout, padding, paddingXY, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onDoubleClick)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Mappers exposing (posixToDateStr, toPosix, toStrPosix)
import Task
import Time exposing (now)


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
    , startDate : Time.Posix
    , zone : Time.Zone
    , tasks : List TaskValue
    , editTask : Maybe EditTask
    }


type EditTask
    = EditTask TaskValue TaskValue


type alias TaskValue =
    { value : String
    , createdDate : Time.Posix
    , editDate : Time.Posix
    , status : TaskStatus
    , taskType : TaskType
    }


type TaskType
    = Single
    | Cron
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
      , startDate = initialDateValue
      , zone = Time.utc
      , tasks =
            [ { value = "first", createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
            , { value = "second", createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
            , { value = "third", createdDate = initialDateValue, editDate = initialDateValue, status = Done, taskType = Single }
            ]
      , editTask = Nothing
      }
    , Task.perform NewTime Time.now
    )


editTaskSample =
    let
        t =
            { value = "second", createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
    in
    EditTask t t


initialDateValue : Time.Posix
initialDateValue =
    newDateRecord 2023 2 22 10 0 0 0 Time.utc
        |> civilToPosix


type Msg
    = Name String
    | SetWindowWidthHeight Int Int
    | NewTime Time.Posix
    | ChangeStartDate String
    | SaveTask
    | CancelEdit
    | EditValue String
    | EditTaskMsg TaskValue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | message = name }, Cmd.none )

        SetWindowWidthHeight width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

        NewTime time ->
            ( { model | startDate = time }, Cmd.none )

        ChangeStartDate startDate ->
            let
                newStartDate =
                    case String.split "-" startDate of
                        year :: month :: day :: _ ->
                            toPosix model.zone model.startDate year month day

                        _ ->
                            model.startDate
            in
            ( { model | startDate = newStartDate }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w h -> SetWindowWidthHeight w h)



-- TIME utils


dateAreTheSame : Time.Posix -> Time.Posix -> Bool
dateAreTheSame time1 time2 =
    let
        t1 =
            posixToCivil time1

        t2 =
            posixToCivil time2
    in
    t1.year == t2.year && t1.month == t2.month && t1.day == t2.day



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


emptyTaskValue : Time.Posix -> TaskValue
emptyTaskValue date =
    { value = "", status = Active, createdDate = date, editDate = date, taskType = Single }


weekDay : Model -> Int -> Element Msg
weekDay model dayDelta =
    let
        weekDayDate =
            addDays dayDelta model.startDate

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
                    [ dayInput model.zone model.startDate
                    , dayTitle (toStrPosix model.zone weekDayDate)
                    ]

            else
                dayTitle (toStrPosix model.zone weekDayDate)

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


filteredTaskPerDay : Time.Posix -> List TaskValue -> List TaskValue
filteredTaskPerDay date tasks =
    let
        filterFunc task =
            dateAreTheSame task.createdDate date
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


dayInput : Time.Zone -> Time.Posix -> Element Msg
dayInput zone time =
    Html.input [ Html.Attributes.type_ "date", Html.Attributes.value (posixToDateStr zone time), Html.Events.onInput ChangeStartDate ] []
        |> html
        |> el [ alignTop ]


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
