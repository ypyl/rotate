port module Sync exposing (..)

import Cron exposing (Cron, fromString)
import Date exposing (Date, fromIsoString, toIsoString)
import Json.Decode as D
import Json.Encode as E
import Mappers exposing (cronToString)
import Model exposing (CronTask, CronTaskStatus(..), Model, PassedCase, SingleTask, SingleTaskStatus(..), SlideTask, SlideTaskStatus(..), TaskValue(..))
import Time exposing (Month(..))
import TimeOnly exposing (TimeOnly)


port setState : E.Value -> Cmd msg


port getState : (D.Value -> msg) -> Sub msg


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "tasks", E.list encodeTaskValue model.tasks ) ]


encodeTaskValue : TaskValue -> E.Value
encodeTaskValue taskValue =
    case taskValue of
        Single item ->
            encodeSingleTaskValue item

        Slide item ->
            encodeSlideTaskValue item

        CronType item ->
            encodeCronTaskValue item


encodeSingleTaskValue : SingleTask -> E.Value
encodeSingleTaskValue taskValue =
    E.object
        [ ( "value", E.string taskValue.value )
        , ( "createdDate", E.string (toIsoString taskValue.createdDate) )
        , ( "editDate", E.string (toIsoString taskValue.editDate) )
        , ( "date", E.string (toIsoString taskValue.date) )
        , ( "status", encodeSingleTaskStatus taskValue.status )
        ]


encodeSlideTaskValue : SlideTask -> E.Value
encodeSlideTaskValue taskValue =
    E.object
        [ ( "value", E.string taskValue.value )
        , ( "createdDate", E.string (toIsoString taskValue.createdDate) )
        , ( "editDate", E.string (toIsoString taskValue.editDate) )
        , ( "startDate", E.string (toIsoString taskValue.startDate) )
        , ( "endDate", E.string (toIsoString taskValue.endDate) )
        , ( "status", encodeSlideTaskStatus taskValue.status )
        ]


encodeCronTaskValue : CronTask -> E.Value
encodeCronTaskValue taskValue =
    E.object
        [ ( "value", E.string taskValue.value )
        , ( "createdDate", E.string (toIsoString taskValue.createdDate) )
        , ( "editDate", E.string (toIsoString taskValue.editDate) )
        , ( "startDate", E.string (toIsoString taskValue.startDate) )
        , ( "endDate", E.string (toIsoString taskValue.endDate) )
        , ( "cron", E.string (cronToString taskValue.cron) )
        , ( "cases", E.list encodePassedCases taskValue.cases )
        ]


encodeSingleTaskStatus : SingleTaskStatus -> E.Value
encodeSingleTaskStatus taskStatus =
    case taskStatus of
        SingleActive ->
            E.string "active"

        SingleDone ->
            E.string "done"

        SingleCancel ->
            E.string "cancel"


encodeCronTaskStatus : CronTaskStatus -> E.Value
encodeCronTaskStatus taskStatus =
    case taskStatus of
        CronDone ->
            E.string "done"

        CronCancel ->
            E.string "cancel"


encodeSlideTaskStatus : SlideTaskStatus -> E.Value
encodeSlideTaskStatus taskStatus =
    case taskStatus of
        SlideActive ->
            E.string "active"

        SlideDone doneDate ->
            E.object
                [ ( "value", E.string "done" )
                , ( "date", E.string (toIsoString doneDate) )
                ]

        SlideCancel cancelDate ->
            E.object
                [ ( "value", E.string "cancel" )
                , ( "date", E.string (toIsoString cancelDate) )
                ]


encodePassedCases : PassedCase -> E.Value
encodePassedCases passedCase =
    E.object
        [ ( "date", E.string (toIsoString passedCase.date) )
        , ( "status", encodeCronTaskStatus passedCase.status )
        ]


modelDecoder : D.Decoder Model
modelDecoder =
    D.map7 Model
        (D.field "windowWidth" D.int)
        (D.field "windowHeight" D.int)
        (D.succeed initialDateValue)
        (D.succeed initialDateValue)
        (D.field "tasks" (D.list taskValueDecoder))
        (D.succeed Nothing)
        (D.succeed Nothing)


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2000 Jan 1


taskValueDecoder : D.Decoder TaskValue
taskValueDecoder =
    D.oneOf
        [ singleTaskDecoder |> D.map (\value -> Single value)
        , slideTaskDecoder |> D.map (\value -> Slide value)
        , cronTaskDecoder |> D.map (\value -> CronType value)
        ]


cronTaskDecoder : D.Decoder CronTask
cronTaskDecoder =
    D.map8 CronTask
        (D.field "value" D.string)
        (D.field "createdDate" D.string |> D.andThen dateDecode)
        (D.field "editDate" D.string |> D.andThen dateDecode)
        (D.field "startDate" D.string |> D.andThen dateDecode)
        (D.field "endDate" D.string |> D.andThen dateDecode)
        (D.field "cron" D.string |> D.andThen cronDecode)
        (D.succeed "")
        (D.field "cases" (D.list passedCaseDecoder))
        |> D.map (\v -> v ( Nothing, Nothing ))


slideTaskDecoder : D.Decoder SlideTask
slideTaskDecoder =
    D.map7 SlideTask
        (D.field "value" D.string)
        (D.field "createdDate" D.string |> D.andThen dateDecode)
        (D.field "editDate" D.string |> D.andThen dateDecode)
        (D.field "startDate" D.string |> D.andThen dateDecode)
        (D.field "endDate" D.string |> D.andThen dateDecode)
        (D.field "status" slideTaskStatusDecoder)
        (D.succeed Nothing)


slideTaskStatusDecoder : D.Decoder SlideTaskStatus
slideTaskStatusDecoder =
    D.oneOf
        [ D.string |> D.andThen (mapToSlideTaskStatus "active")
        , D.map2 (\_ date -> SlideDone date)
            (D.field "value" (D.string |> D.andThen (mapToSlideTaskStatus "done")))
            (D.field "date" D.string |> D.andThen dateDecode)
        , D.map2 (\_ date -> SlideCancel date)
            (D.field "value" (D.string |> D.andThen (mapToSlideTaskStatus "cancel")))
            (D.field "date" D.string |> D.andThen dateDecode)
        ]


mapToSlideTaskStatus : String -> String -> D.Decoder SlideTaskStatus
mapToSlideTaskStatus statusValue value =
    if value == statusValue then
        D.succeed SlideActive

    else
        D.fail <| "Not able to parse task status: " ++ value


singleTaskDecoder : D.Decoder SingleTask
singleTaskDecoder =
    D.map6 SingleTask
        (D.field "value" D.string)
        (D.field "createdDate" D.string |> D.andThen dateDecode)
        (D.field "editDate" D.string |> D.andThen dateDecode)
        (D.field "date" D.string |> D.andThen dateDecode)
        (D.field "status" singleTaskStatusDecoder)
        (D.maybe (D.field "time" D.string |> D.andThen timeOnlyDecode))


singleTaskStatusDecoder : D.Decoder SingleTaskStatus
singleTaskStatusDecoder =
    D.string |> D.andThen mapToSingleTaskStatus


mapToSingleTaskStatus : String -> D.Decoder SingleTaskStatus
mapToSingleTaskStatus value =
    if value == "active" then
        D.succeed SingleActive

    else if value == "done" then
        D.succeed SingleDone

    else if value == "cancel" then
        D.succeed SingleCancel

    else
        D.fail <| "Not able to parse task status: " ++ value


dateDecode : String -> D.Decoder Date
dateDecode dateString =
    case fromIsoString dateString of
        Ok d ->
            D.succeed d

        Err error ->
            D.fail error


cronDecode : String -> D.Decoder Cron
cronDecode crontString =
    case fromString ("* * " ++ crontString) of
        Ok d ->
            D.succeed d

        Err _ ->
            D.fail <| "Not able to parse " ++ crontString


passedCaseDecoder : D.Decoder PassedCase
passedCaseDecoder =
    D.map2 PassedCase
        (D.field "date" D.string |> D.andThen dateDecode)
        (D.field "status" cronTaskStatusDecoder)


cronTaskStatusDecoder : D.Decoder CronTaskStatus
cronTaskStatusDecoder =
    D.string |> D.andThen mapToCronTaskStatus


mapToCronTaskStatus : String -> D.Decoder CronTaskStatus
mapToCronTaskStatus value =
    if value == "done" then
        D.succeed CronDone

    else if value == "cancel" then
        D.succeed CronCancel

    else
        D.fail <| "Not able to parse task status: " ++ value

timeOnlyDecode : String -> D.Decoder TimeOnly
timeOnlyDecode timeOnlyString =
    case TimeOnly.fromString timeOnlyString of
        Ok d ->
            D.succeed d

        Err _ ->
            D.fail <| "Not able to parse " ++ timeOnlyString
