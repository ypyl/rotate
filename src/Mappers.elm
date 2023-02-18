module Mappers exposing (..)

import Derberos.Date.Delta exposing (addDays, addMonths, addYears)
import Time exposing (Month)


toIntStrMonth : Month -> String
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


toStrMonth : Month -> String
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


toStrPosix : Time.Zone -> Time.Posix -> String
toStrPosix zone time =
    String.fromInt (Time.toDay zone time) ++ ", " ++ toStrMonth (Time.toMonth zone time) ++ " " ++ String.fromInt (Time.toYear zone time)


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
