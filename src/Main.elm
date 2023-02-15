module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Derberos.Date.Core exposing (civilToPosix, newDateRecord, posixToCivil)
import Derberos.Date.Delta exposing (addDays, addMonths, addYears)
import Element exposing (Element, alignRight, centerX, centerY, el, fill, height, inFront, padding, paddingXY, px, rgb255, rgba255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import Time


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
                Just t ->
                    [ grayBackground t |> inFront ]

                Nothing ->
                    []
    in
    Element.layout modalWindow (daysView model)


type alias Model =
    { message : String
    , windowWidth : Int
    , windowHeight : Int
    , startDate : Time.Posix
    , zone : Time.Zone
    , tasks : List TodoTask
    , editTask : Maybe TaskValue
    }


type TodoTask
    = View TaskValue
    | Edit TaskValue


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
            [ View { value = "first", createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
            , View { value = "second", createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
            , View { value = "third", createdDate = initialDateValue, editDate = initialDateValue, status = Done, taskType = Single }
            ]
      , editTask = Just { value = "second", createdDate = initialDateValue, editDate = initialDateValue, status = Active, taskType = Single }
      }
    , Task.perform NewTime Time.now
    )


initialDateValue : Time.Posix
initialDateValue =
    newDateRecord 2023 2 16 10 0 0 0 Time.utc
        |> civilToPosix


type Msg
    = Name String
    | SetWindowWidthHeight Int Int
    | NewTime Time.Posix
    | ChangeStartDate String
    | SaveTask TaskValue
    | CancelEdit


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

        SaveTask _ ->
            ( model, Cmd.none )

        CancelEdit ->
            ( { model | editTask = Nothing }, Cmd.none )


toPosix : Time.Zone -> Time.Posix -> String -> String -> String -> Time.Posix
toPosix zone default yearStr monthStr dayStr =
    let
        year =
            String.toInt yearStr

        -- TODO additional check > 0 && < 13
        month =
            String.toInt monthStr

        -- TODO additional check for day?
        day =
            String.toInt dayStr
    in
    case ( year, month, day ) of
        ( Just y, Just m, Just d ) ->
            Time.millisToPosix 0 |> addYears (y - 1970) |> addMonths (m - 1) zone |> addDays (d - 1)

        _ ->
            default


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


toStrPosix : Time.Zone -> Time.Posix -> String
toStrPosix zone time =
    let
        toStrMonth : Time.Month -> String
        toStrMonth month =
            case month of
                Time.Jan ->
                    "Jan"

                Time.Feb ->
                    "Feb"

                Time.Mar ->
                    "Mar"

                Time.Apr ->
                    "Apr"

                Time.May ->
                    "May"

                Time.Jun ->
                    "Jun"

                Time.Jul ->
                    "Jul"

                Time.Aug ->
                    "Aug"

                Time.Sep ->
                    "Sep"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"
    in
    String.fromInt (Time.toDay zone time) ++ ", " ++ toStrMonth (Time.toMonth zone time) ++ " " ++ String.fromInt (Time.toYear zone time)



-- UI ELEMENTS


dayTitleSize : number
dayTitleSize =
    30


taskSize : number
taskSize =
    20


dayTitle : String -> Element msg
dayTitle value =
    el [ Font.bold, height (Element.px dayTitleSize), centerX ] (text value)


taskValueView : TaskStatus -> String -> Element msg
taskValueView status value =
    let
        extraAttr =
            case status of
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
         , height (Element.px taskSize)
         , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
         , Border.color (rgb255 0 0 0)
         , paddingXY 3 0
         ]
            ++ extraAttr
        )
        (text value)


weekDayWidth : number
weekDayWidth =
    250


weekDay : Model -> Int -> Element Msg
weekDay model dayDelta =
    let
        weekDayDate =
            addDays dayDelta model.startDate

        weekDayTasks =
            filteredTaskPerDay weekDayDate model.tasks

        emptyTasks count =
            List.repeat count (taskValueView Active "")

        totalCountOfTasksToShow =
            numberOfTaskToShow model.windowHeight

        emptyTaskCount =
            if totalCountOfTasksToShow - List.length weekDayTasks > 0 then
                totalCountOfTasksToShow - List.length weekDayTasks

            else
                0

        title =
            if dayDelta == 0 then
                Element.row [ width fill, spacing 5 ]
                    [ dayInput model.zone model.startDate
                    , dayTitle (toStrPosix model.zone weekDayDate)
                    ]

            else
                dayTitle (toStrPosix model.zone weekDayDate)

        viewTask task =
            case task of
                View t ->
                    taskValueView t.status t.value

                Edit _ ->
                    Debug.todo "Edit task view"
    in
    Element.column
        [ Font.color (rgb255 0 0 0)
        , Border.color (rgb255 0 0 0)

        --, Border.width 1
        , Border.rounded 3
        , padding 3
        , width (Element.px weekDayWidth)
        ]
        (title :: List.map viewTask weekDayTasks ++ emptyTasks emptyTaskCount)



-- TODO use dict to find tasks for days (there will be a special case for different types)


filteredTaskPerDay : Time.Posix -> List TodoTask -> List TodoTask
filteredTaskPerDay date tasks =
    let
        filterFunc task =
            case task of
                View t ->
                    dateAreTheSame t.createdDate date

                Edit t ->
                    dateAreTheSame t.createdDate date
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
        |> Element.html
        |> el [ Element.alignTop ]


posixToDateStr : Time.Zone -> Time.Posix -> String
posixToDateStr zone time =
    String.fromInt (Time.toYear zone time)
        ++ "-"
        ++ toIntStrMonth (Time.toMonth zone time)
        ++ "-"
        ++ adjustDay (Time.toDay zone time)


adjustDay : Int -> String
adjustDay val =
    if val < 10 then
        "0" ++ String.fromInt val

    else
        String.fromInt val


toIntStrMonth : Time.Month -> String
toIntStrMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


grayColorbackground : Element.Color
grayColorbackground =
    rgba255 107 114 128 0.6


grayBackground : TaskValue -> Element Msg
grayBackground taskValue =
    el
        [ width fill
        , height fill
        , Background.color grayColorbackground
        ]
        (modal taskValue)


modal : TaskValue -> Element Msg
modal taskValue =
    el
        [ centerX
        , centerY
        , width (px (weekDayWidth * 2))
        , Background.color white
        , Border.rounded 5
        ]
        (modalFooter taskValue)


modalFooter : TaskValue -> Element Msg
modalFooter taskValue =
    Element.column [ width fill, padding 10 ]
        [ text "Some data"
        , row [ width fill, spacing 10 ] [ cancelButton, saveButton taskValue ] ]


gray : Element.Color
gray =
    rgb255 107 114 128


grayFocused : Element.Color
grayFocused =
    rgb255 55 65 81


blue : Element.Color
blue =
    Element.rgb255 59 139 246

white : Element.Color
white =
    Element.rgb255 255 255 255

blueFocused : Element.Color
blueFocused =
    rgb255 29 78 216


saveButton : TaskValue -> Element Msg
saveButton taskValue =
    button
        [ Background.color blue
        , alignRight
        , padding 10
        , Border.rounded 5
        , Font.color white
        , Element.focused
            [ Background.color blueFocused ]
        ]
        { onPress = Just (SaveTask taskValue)
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
        , Element.focused
            [ Background.color grayFocused ]
        ]
        { onPress = Just CancelEdit
        , label = text "Cancel"
        }
