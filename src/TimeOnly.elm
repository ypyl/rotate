module TimeOnly exposing (..)

import Parser exposing ((|.), (|=), DeadEnd, Parser)


type TimeOnly
    = TimeOnly Int


add : TimeOnly -> TimeOnly -> TimeOnly
add (TimeOnly v1) (TimeOnly v2) =
    v1 + v2 |> modBy 1440 |> TimeOnly


minus : TimeOnly -> TimeOnly -> TimeOnly
minus (TimeOnly v1) (TimeOnly v2) =
    v1 - v2 |> modBy 1440 |> TimeOnly


hours : Int -> TimeOnly
hours t =
    t |> modBy 24 |> (*) 60 |> TimeOnly


minutes : Int -> TimeOnly
minutes t =
    t |> modBy 1440 |> TimeOnly


toString : TimeOnly -> String
toString (TimeOnly value) =
    let
        tr =
            hmRecordFromSeconds value
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)


getHours : TimeOnly -> Int
getHours timeOnly =
    case timeOnly of
        TimeOnly t ->
            t // 60


getMinutes : TimeOnly -> Int
getMinutes timeOnly =
    case timeOnly of
        TimeOnly t ->
            modBy 60 t


hmRecordFromSeconds : Int -> HMRecord
hmRecordFromSeconds s =
    { minutes = modBy 60 s, hours = s // 60 }


type alias HMRecord =
    { hours : Int, minutes : Int }


fromString : String -> Result (List DeadEnd) TimeOnly
fromString s =
    Parser.run hmParser s


hmParser : Parser TimeOnly
hmParser =
    (Parser.succeed HMRecord
        |= Parser.oneOf [ altIntParser, Parser.int ]
        |. Parser.symbol ":"
        |= Parser.oneOf [ altIntParser, Parser.int ]
    )
        |> Parser.map typedTimeFromHMRecord


altIntParser : Parser Int
altIntParser =
    Parser.symbol "0" |> Parser.andThen (\_ -> Parser.int)


typedTimeFromHMRecord : HMRecord -> TimeOnly
typedTimeFromHMRecord r =
    add (hours r.hours) (minutes r.minutes)
