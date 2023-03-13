port module Sync exposing (..)

import Cron exposing (Cron, fromString)
import Date exposing (Date, fromIsoString, toIsoString)
import Json.Decode as D
import Json.Encode as E
import Mappers exposing (cronToString)
import Model exposing (CronTaskValue, Model, PassedCase, SlideTaskValue, TaskStatus(..), TaskType(..), TaskValue)
import Time exposing (Month(..))


port setState : E.Value -> Cmd msg


port getState : (D.Value -> msg) -> Sub msg


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "tasks", E.list encodeTaskValue model.tasks ) ]


encodeTaskValue : TaskValue -> E.Value
encodeTaskValue taskValue =
    E.object
        [ ( "value", E.string taskValue.value )
        , ( "createdDate", E.string (toIsoString taskValue.createdDate) )
        , ( "editDate", E.string (toIsoString taskValue.editDate) )
        , ( "date", E.string (toIsoString taskValue.date) )
        , ( "taskType", encodeTaskType taskValue.taskType )
        ]


encodeTaskType : TaskType -> E.Value
encodeTaskType taskType =
    case taskType of
        Single taskStatus ->
            encodeTaskStatus taskStatus

        Slide slideTaskValue ->
            encodeSlideTaskValue slideTaskValue

        CronType cronTaskValue ->
            encodeCrontTaskValue cronTaskValue


encodeTaskStatus : TaskStatus -> E.Value
encodeTaskStatus taskStatus =
    case taskStatus of
        Active ->
            E.string "active"

        Done ->
            E.string "done"

        Cancel ->
            E.string "cancel"


encodeSlideTaskValue : SlideTaskValue -> E.Value
encodeSlideTaskValue slideTaskValue =
    E.object
        [ ( "endDate", E.string (toIsoString slideTaskValue.endDate) )
        , ( "status", encodeTaskStatus slideTaskValue.status )
        ]


encodeCrontTaskValue : CronTaskValue -> E.Value
encodeCrontTaskValue cronTaskValue =
    E.object
        [ ( "endDate", E.string (toIsoString cronTaskValue.endDate) )
        , ( "cron", E.string (cronToString cronTaskValue.cron) )
        , ( "cases", E.list encodePassedCases cronTaskValue.cases )
        ]


encodePassedCases : PassedCase -> E.Value
encodePassedCases passedCase =
    E.object
        [ ( "date", E.string (toIsoString passedCase.date) )
        , ( "status", encodeTaskStatus passedCase.status )
        ]


modelDecoder : D.Decoder Model
modelDecoder =
    D.map7 Model
        (D.field "windowWidth" D.int)
        (D.field "windowHeight" D.int)
        (D.succeed initialDateValue)
        (D.succeed initialDateValue)
        (D.field "tasks" (D.list taskDecoder))
        (D.succeed Nothing)
        (D.succeed Nothing)


initialDateValue : Date
initialDateValue =
    Date.fromCalendarDate 2023 Mar 12


taskDecoder : D.Decoder TaskValue
taskDecoder =
    D.map6 TaskValue
        (D.field "value" D.string)
        (D.field "createdDate" D.string |> D.andThen dateDecode)
        (D.field "editDate" D.string |> D.andThen dateDecode)
        (D.field "date" D.string |> D.andThen dateDecode)
        (D.field "taskType" taskTypeDecoder)
        (D.field "error" (D.succeed []))


dateDecode : String -> D.Decoder Date
dateDecode dateString =
    case fromIsoString dateString of
        Ok d ->
            D.succeed d

        Err error ->
            D.fail error


taskTypeDecoder : D.Decoder TaskType
taskTypeDecoder =
    D.oneOf
        [ taskStatusDecoder |> D.map (\value -> Single value)
        , slideDecoder |> D.map (\value -> Slide value)
        , cronTypeDecoder |> D.map (\value -> CronType value)
        ]


taskStatusDecoder : D.Decoder TaskStatus
taskStatusDecoder =
    D.string |> D.andThen mapToTaskStatus


mapToTaskStatus : String -> D.Decoder TaskStatus
mapToTaskStatus value =
    if value == "active" then
        D.succeed Active

    else if value == "done" then
        D.succeed Done

    else if value == "cancel" then
        D.succeed Cancel

    else
        D.fail <| "Not able to parse task status: " ++ value


slideDecoder : D.Decoder SlideTaskValue
slideDecoder =
    D.map2 SlideTaskValue
        (D.field "endDate" D.string |> D.andThen dateDecode)
        (D.field "status" taskStatusDecoder)


cronTypeDecoder : D.Decoder CronTaskValue
cronTypeDecoder =
    D.map4 CronTaskValue
        (D.field "cron" D.string |> D.andThen cronDecode)
        (D.succeed "")
        (D.field "endDate" D.string |> D.andThen dateDecode)
        (D.field "cases" (D.list passedCaseDecoder))


cronDecode : String -> D.Decoder Cron
cronDecode crontString =
    case fromString crontString of
        Ok d ->
            D.succeed d

        Err _ ->
            D.fail <| "Not able to parse " ++ crontString


passedCaseDecoder : D.Decoder PassedCase
passedCaseDecoder =
    D.map2 PassedCase
        (D.field "date" D.string |> D.andThen dateDecode)
        (D.field "status" taskStatusDecoder)
