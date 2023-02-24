module Mappers exposing (cronToString)

import Cron exposing (Atom(..), Cron(..), Expr(..), Month(..), Term(..), WeekDay(..))


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
