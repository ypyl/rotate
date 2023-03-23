module Mappers exposing (..)

import Cron exposing (Atom(..), Cron(..), Expr(..), Month(..), Term(..), WeekDay(..))
import Date exposing (Date, day, month, monthToNumber, weekday, weekdayToNumber)
import Model exposing (CronTask, CronTaskStatus(..), SingleTask, SingleTaskStatus(..), SlideTask, SlideTaskStatus(..), TaskStatus(..))
import Model exposing (TaskValue)


cronToString : Cron -> String
cronToString (Cron _ _ day month week) =
    [ exprIntToString day, exprMonthToString month, exprWeekDayToString week ] |> String.join " "


exprIntToString : Expr Int -> String
exprIntToString expr =
    case expr of
        Single term ->
            termIntToString term

        Multiple items ->
            items |> List.map termIntToString |> String.join ","

        Every ->
            "*"


exprMonthToString : Expr Month -> String
exprMonthToString expr =
    case expr of
        Single term ->
            termMonthToString term

        Multiple items ->
            items |> List.map termMonthToString |> String.join ","

        Every ->
            "*"


exprWeekDayToString : Expr WeekDay -> String
exprWeekDayToString expr =
    case expr of
        Single term ->
            termWeekDayToString term

        Multiple items ->
            items |> List.map termWeekDayToString |> String.join ","

        Every ->
            "*"


termIntToString : Term Int -> String
termIntToString term =
    case term of
        Step atom value ->
            atomIntToString atom ++ "/" ++ String.fromInt value

        EveryStep value ->
            "*/" ++ String.fromInt value

        Atom atom ->
            atomIntToString atom


termMonthToString : Term Month -> String
termMonthToString term =
    case term of
        Step atom value ->
            atomMonthToString atom ++ "/" ++ String.fromInt value

        EveryStep value ->
            "*/" ++ String.fromInt value

        Atom atom ->
            atomMonthToString atom


termWeekDayToString : Term WeekDay -> String
termWeekDayToString term =
    case term of
        Step atom value ->
            atomWeekDayToString atom ++ "/" ++ String.fromInt value

        EveryStep value ->
            "*/" ++ String.fromInt value

        Atom atom ->
            atomWeekDayToString atom


atomIntToString : Atom Int -> String
atomIntToString atom =
    case atom of
        Particle value ->
            String.fromInt value

        Range start end ->
            String.fromInt start ++ "-" ++ String.fromInt end


atomMonthToString : Atom Month -> String
atomMonthToString atom =
    case atom of
        Particle value ->
            monthToString value

        Range start end ->
            monthToString start ++ "-" ++ monthToString end


atomWeekDayToString : Atom WeekDay -> String
atomWeekDayToString atom =
    case atom of
        Particle value ->
            weekdayToString value

        Range start end ->
            weekdayToString start ++ "-" ++ weekdayToString end


monthToString : Month -> String
monthToString month =
    case month of
        January ->
            "1"

        February ->
            "2"

        March ->
            "3"

        April ->
            "4"

        May ->
            "5"

        June ->
            "6"

        July ->
            "7"

        August ->
            "8"

        September ->
            "9"

        October ->
            "10"

        November ->
            "11"

        December ->
            "12"


weekdayToString : Cron.WeekDay -> String
weekdayToString weekDay =
    case weekDay of
        Sunday ->
            "0"

        Monday ->
            "1"

        Tuesday ->
            "2"

        Wednesday ->
            "3"

        Thursday ->
            "4"

        Friday ->
            "5"

        Saturday ->
            "6"



-- TODO cron matches date


isCronMatchDate : Cron -> Date -> Bool
isCronMatchDate cron date =
    let
        dateWeekday =
            weekday date |> weekdayToNumber

        dateMonth =
            month date |> monthToNumber

        dateDay =
            day date

        (Cron _ _ cronDay cronMonth cronWeekDay) =
            cron
    in
    isMatchExprInt cronDay dateDay && isMatchExprMonth cronMonth dateMonth && isMatchExprWeekDay cronWeekDay dateWeekday


isMatchExprWeekDay : Expr WeekDay -> Int -> Bool
isMatchExprWeekDay term value =
    case term of
        Single termValue ->
            isMatchTermWeekDay value termValue

        Multiple terms ->
            List.any (isMatchTermWeekDay value) terms

        Every ->
            True


isMatchTermWeekDay : Int -> Term WeekDay -> Bool
isMatchTermWeekDay value term =
    case term of
        Step atom step ->
            isMatchAtomWeekDay atom value && remainderBy step value == 0

        EveryStep step ->
            remainderBy step value == 0

        Atom atom ->
            isMatchAtomWeekDay atom value


isMatchAtomWeekDay : Atom WeekDay -> Int -> Bool
isMatchAtomWeekDay atom value =
    case atom of
        Particle i ->
            weekdayToInt i == value

        Range start end ->
            weekdayToInt start <= value && value <= weekdayToInt end


isMatchExprMonth : Expr Month -> Int -> Bool
isMatchExprMonth term value =
    case term of
        Single termValue ->
            isMatchTermMonth value termValue

        Multiple terms ->
            List.all (isMatchTermMonth value) terms

        Every ->
            True


isMatchTermMonth : Int -> Term Month -> Bool
isMatchTermMonth value term =
    case term of
        Step atom step ->
            isMatchAtomMonth atom value && remainderBy step value == 0

        EveryStep step ->
            remainderBy step value == 0

        Atom atom ->
            isMatchAtomMonth atom value


isMatchAtomMonth : Atom Month -> Int -> Bool
isMatchAtomMonth atom value =
    case atom of
        Particle i ->
            monthToInt i == value

        Range start end ->
            monthToInt start <= value && value <= monthToInt end


isMatchExprInt : Expr Int -> Int -> Bool
isMatchExprInt term value =
    case term of
        Single termValue ->
            isMatchTermInt value termValue

        Multiple terms ->
            List.all (isMatchTermInt value) terms

        Every ->
            True


isMatchTermInt : Int -> Term Int -> Bool
isMatchTermInt value term =
    case term of
        Step atom step ->
            isMatchAtomInt atom value && remainderBy step value == 0

        EveryStep step ->
            remainderBy step value == 0

        Atom atom ->
            isMatchAtomInt atom value


isMatchAtomInt : Atom Int -> Int -> Bool
isMatchAtomInt atom value =
    case atom of
        Particle i ->
            i == value

        Range start end ->
            start <= value && value <= end


weekdayToInt : Cron.WeekDay -> Int
weekdayToInt weekDay =
    case weekDay of
        Sunday ->
            0

        Monday ->
            1

        Tuesday ->
            2

        Wednesday ->
            3

        Thursday ->
            4

        Friday ->
            5

        Saturday ->
            6


monthToInt : Month -> Int
monthToInt month =
    case month of
        January ->
            1

        February ->
            2

        March ->
            3

        April ->
            4

        May ->
            5

        June ->
            6

        July ->
            7

        August ->
            8

        September ->
            9

        October ->
            10

        November ->
            11

        December ->
            12


mapTaskStatusToSingleTaskStatus : TaskStatus -> SingleTaskStatus
mapTaskStatusToSingleTaskStatus status =
    case status of
        Active ->
            SingleActive

        Done ->
            SingleDone

        Cancel ->
            SingleCancel


mapSingleTaskStatusToTaskStatus : SingleTaskStatus -> TaskStatus
mapSingleTaskStatusToTaskStatus status =
    case status of
        SingleActive ->
            Active

        SingleDone ->
            Done

        SingleCancel ->
            Cancel


mapTaskStatusToSlideTaskStatus : TaskStatus -> Date -> SlideTaskStatus
mapTaskStatusToSlideTaskStatus status statusChangedDate =
    case status of
        Active ->
            SlideActive

        Done ->
            SlideDone statusChangedDate

        Cancel ->
            SlideCancel statusChangedDate


mapSlideTaskStatusToTaskStatus : SlideTaskStatus -> TaskStatus
mapSlideTaskStatusToTaskStatus status =
    case status of
        SlideActive ->
            Active

        SlideDone _ ->
            Done

        SlideCancel _ ->
            Cancel


mapCronTaskStatusToTaskStatus : CronTaskStatus -> TaskStatus
mapCronTaskStatusToTaskStatus status =
    case status of
        CronDone ->
            Done

        CronCancel ->
            Cancel


mapSingleTaskStatusToSlideTaskStatus : Date -> SingleTaskStatus -> SlideTaskStatus
mapSingleTaskStatusToSlideTaskStatus date status =
    case status of
        SingleActive ->
            SlideActive

        SingleDone ->
            SlideDone date

        SingleCancel ->
            SlideCancel date


mapSingleTaskToSlide : SingleTask -> SlideTask
mapSingleTaskToSlide singleTask =
    SlideTask
        singleTask.value
        singleTask.createdDate
        singleTask.editDate
        singleTask.date
        singleTask.date
        (mapSingleTaskStatusToSlideTaskStatus singleTask.date singleTask.status)
        Nothing


mapSingleTaskToCron : SingleTask -> CronTask
mapSingleTaskToCron singleTask =
    CronTask
        singleTask.value
        singleTask.createdDate
        singleTask.editDate
        singleTask.date
        singleTask.date
        (Cron Every Every Every Every Every)
        ""
        []
        ( Nothing, Nothing )


mapSlideTaskStatusToSingleTaskStatus : SlideTaskStatus -> SingleTaskStatus
mapSlideTaskStatusToSingleTaskStatus status =
    case status of
        SlideActive ->
            SingleActive

        SlideDone _ ->
            SingleDone

        SlideCancel _ ->
            SingleCancel


mapSlideTaskToSingle : SlideTask -> SingleTask
mapSlideTaskToSingle slideTask =
    SingleTask
        slideTask.value
        slideTask.createdDate
        slideTask.editDate
        slideTask.startDate
        (mapSlideTaskStatusToSingleTaskStatus slideTask.status)


mapSlideTaskToCron : SlideTask -> CronTask
mapSlideTaskToCron slideTask =
    CronTask
        slideTask.value
        slideTask.createdDate
        slideTask.editDate
        slideTask.startDate
        slideTask.endDate
        (Cron Every Every Every Every Every)
        ""
        []
        ( Nothing, Nothing )


mapCronTaskToSingle : CronTask -> SingleTask
mapCronTaskToSingle cronTask =
    SingleTask
        cronTask.value
        cronTask.createdDate
        cronTask.editDate
        cronTask.startDate
        SingleActive


mapCronTaskToSlide : CronTask -> SlideTask
mapCronTaskToSlide cronTask =
    SlideTask
        cronTask.value
        cronTask.createdDate
        cronTask.editDate
        cronTask.startDate
        cronTask.endDate
        SlideActive
        Nothing
