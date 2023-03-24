module DatePicker exposing (Model, Msg, create, getLastSelectedDate, update, view)

import Browser.Dom exposing (Element)
import Colors exposing (black, blue, gray, white)
import Date exposing (Date, Interval(..), Month, Unit(..), add, day, fromCalendarDate, month, range, weekday, year)
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, fill, none, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Time exposing (Month(..), Weekday(..))


type Model
    = Model InternalModel


type alias InternalModel =
    { today : Date
    , viewStatus : ViewStatus
    , lastSelectedDate : Maybe Date
    }


type alias Year =
    Int


type ViewStatus
    = DayView Month Year
    | MonthView Year
    | YearView Year


create : Date -> Model
create today =
    Model { today = today, viewStatus = DayView (month today) (year today), lastSelectedDate = Nothing }


extractModel : Model -> InternalModel
extractModel model =
    case model of
        Model subModel ->
            subModel


getLastSelectedDate : Model -> Maybe Date
getLastSelectedDate model =
    extractModel model |> .lastSelectedDate


type Msg
    = ShowMonthView Year
    | ShowYearView Year
    | ShowDayView Month Year
    | SelectDate Date
    | Next
    | Previous


update : Msg -> Model -> Model
update msg model =
    extractModel model |> updateInternal msg |> Model


updateInternal : Msg -> InternalModel -> InternalModel
updateInternal msg model =
    case msg of
        ShowMonthView year ->
            { model | viewStatus = MonthView year }

        ShowYearView year ->
            { model | viewStatus = YearView year }

        ShowDayView month year ->
            { model | viewStatus = DayView month year }

        SelectDate date ->
            { model | lastSelectedDate = Just date }

        Next ->
            case model.viewStatus of
                DayView monthValue yearValue ->
                    let
                        netStartDate =
                            add Months 1 (fromCalendarDate yearValue monthValue 1)
                    in
                    { model | viewStatus = DayView (month netStartDate) (year netStartDate) }

                MonthView yearValue ->
                    let
                        netStartDate =
                            add Years 1 (fromCalendarDate yearValue Jan 1)
                    in
                    { model | viewStatus = MonthView (year netStartDate) }

                YearView yearValue ->
                    let
                        netStartDate =
                            add Years 12 (fromCalendarDate yearValue Jan 1)
                    in
                    { model | viewStatus = YearView (year netStartDate) }

        Previous ->
            case model.viewStatus of
                DayView monthValue yearValue ->
                    let
                        netStartDate =
                            add Months -1 (fromCalendarDate yearValue monthValue 1)
                    in
                    { model | viewStatus = DayView (month netStartDate) (year netStartDate) }

                MonthView yearValue ->
                    let
                        netStartDate =
                            add Years -1 (fromCalendarDate yearValue Jan 1)
                    in
                    { model | viewStatus = MonthView (year netStartDate) }

                YearView yearValue ->
                    let
                        netStartDate =
                            add Years -12 (fromCalendarDate yearValue Jan 1)
                    in
                    { model | viewStatus = YearView (year netStartDate) }


view : Model -> Element Msg
view model =
    extractModel model |> viewInternal


viewInternal : InternalModel -> Element Msg
viewInternal model =
    case model.viewStatus of
        DayView month year ->
            dayView model.today month year

        MonthView year ->
            monthView model.today year

        YearView year ->
            yearView model.today year


spacingValue : number
spacingValue =
    10


cellWidth : number
cellWidth =
    30


dayView : Date -> Month -> Year -> Element Msg
dayView today month year =
    let
        startDate =
            Date.fromCalendarDate year month 1

        delta =
            weekday startDate |> deltaBasedOnWeekDay

        initialDate =
            add Days delta startDate
    in
    column [ Background.color white, Border.color black, Border.width 1, Border.rounded 5, spacing spacingValue, padding 5 ]
        ([ row [ width fill ]
            [ el [ width (px cellWidth), alignLeft ] (button [ width fill ] { onPress = Just Previous, label = el [ centerX ] (text "<") })
            , el [ width fill, centerX ] (button [ width fill ] { onPress = Just (ShowMonthView year), label = el [ centerX ] (text (monthToStr month)) })
            , el [ width (px cellWidth), alignRight ] (button [ width fill ] { onPress = Just Next, label = el [ centerX ] (text ">") })
            ]
         , row [ spacing spacingValue, width fill ] ([ "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" ] |> List.map text |> List.map (el [ centerX ]) |> List.map (el [ width (px cellWidth), Font.bold ]))
         , row [ spacing spacingValue, width fill ] (generateDaysRow today initialDate month)
         ]
            ++ generateRowsForMonth today month (add Days 7 initialDate)
        )


generateDaysRow : Date -> Date -> Month -> List (Element Msg)
generateDaysRow today date currentMonth =
    let
        txtColor d =
            if month d == currentMonth && today == d then
                blue

            else if month d == currentMonth then
                black

            else
                gray

        dayBtn d =
            button [ width (px cellWidth) ] { onPress = Just (SelectDate d), label = el [ centerX, Font.color (txtColor d) ] (text (day d |> String.fromInt)) }
    in
    range Day 1 date (add Days 7 date) |> List.map dayBtn


generateRowsForMonth : Date -> Month -> Date -> List (Element Msg)
generateRowsForMonth today monthValue startDate =
    let
        internalGenerator dateValue result =
            if month dateValue == monthValue then
                row [ spaceEvenly, width fill ] (generateDaysRow today dateValue monthValue) :: internalGenerator (add Days 7 dateValue) result

            else
                result
    in
    internalGenerator startDate []


monthView : Date -> Year -> Element Msg
monthView today yearValue =
    let
        fontColor m =
            if m == month today && yearValue == year today then
                blue

            else
                black

        monthBtn m =
            button [ width (px 60), Font.color (fontColor m) ] { onPress = Just (ShowDayView m yearValue), label = el [ centerX ] (text (monthToShortStr m)) }
    in
    column [ Background.color white, Border.color black, Border.width 1, Border.rounded 5, spacing 20, padding 5 ]
        [ row [ width fill ]
            [ el [ width (px cellWidth), alignLeft ] (button [ width fill ] { onPress = Just Previous, label = el [ centerX ] (text "<") })
            , el [ width fill, centerX ] (button [ width fill ] { onPress = Just (ShowYearView yearValue), label = el [ centerX ] (text (String.fromInt yearValue)) })
            , el [ width (px cellWidth), alignRight ] (button [ width fill ] { onPress = Just Next, label = el [ centerX ] (text ">") })
            ]
        , row [ spacing spacingValue, width fill ] ([ Jan, Feb, Mar, Apr ] |> List.map monthBtn)
        , row [ spacing spacingValue, width fill ] ([ May, Jun, Jul, Aug ] |> List.map monthBtn)
        , row [ spacing spacingValue, width fill ] ([ Sep, Oct, Nov, Dec ] |> List.map monthBtn)
        ]


yearView : Date -> Year -> Element Msg
yearView today yearValue =
    let
        fontColor y =
            if y == year today then
                blue

            else
                black

        startYear =
            yearValue - 5

        yearBtn y =
            button [ width (px 60) ] { onPress = Just (ShowMonthView y), label = el [ centerX, Font.color (fontColor y) ] (text (String.fromInt y)) }
    in
    column [ Background.color white, Border.color black, Border.width 1, Border.rounded 5, spacing 20, padding 5 ]
        [ row [ width fill ]
            [ el [ width (px cellWidth), alignLeft ] (button [ width fill ] { onPress = Just Previous, label = el [ centerX ] (text "<") })
            , el [ width fill, centerX ] none
            , el [ width (px cellWidth), alignRight ] (button [ width fill ] { onPress = Just Next, label = el [ centerX ] (text ">") })
            ]
        , row [ spacing spacingValue, width fill ] (List.range startYear (startYear + 3) |> List.map yearBtn)
        , row [ spacing spacingValue, width fill ] (List.range (startYear + 4) (startYear + 7) |> List.map yearBtn)
        , row [ spacing spacingValue, width fill ] (List.range (startYear + 8) (startYear + 11) |> List.map yearBtn)
        ]


deltaBasedOnWeekDay : Weekday -> Int
deltaBasedOnWeekDay weekdayValue =
    case weekdayValue of
        Sun ->
            0

        Mon ->
            -1

        Tue ->
            -2

        Wed ->
            -3

        Thu ->
            -4

        Fri ->
            -5

        Sat ->
            -6


monthToStr : Month -> String
monthToStr month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


monthToShortStr : Month -> String
monthToShortStr month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
