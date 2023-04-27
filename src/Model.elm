module Model exposing (..)

import Cron exposing (Cron)
import Date exposing (Date)
import DatePicker as DT
import TimeOnly exposing (TimeOnly)


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , today : Date
    , startDate : Date
    , tasks : List TaskValue
    , editTask : Maybe EditTask
    , dt : Maybe ( DatePickerType, DT.Model, Bool )
    }


type DatePickerType
    = StartDate
    | EditStartDate
    | EditEndDate


type EditTask
    = EditTask Date TaskValue TaskValue


type TaskValue
    = Single SingleTask
    | Slide SlideTask
    | CronType CronTask


type alias SingleTask =
    { value : String
    , createdDate : Date
    , editDate : Date
    , date : Date
    , status : SingleTaskStatus
    , time : Maybe TimeOnly
    }


type alias SlideTask =
    { value : String
    , createdDate : Date
    , editDate : Date
    , startDate : Date
    , endDate : Date
    , status : SlideTaskStatus
    , error : Maybe String
    }


type alias CronTask =
    { value : String
    , createdDate : Date
    , editDate : Date
    , startDate : Date
    , endDate : Date
    , cron : Cron
    , cronEditValue : String
    , cases : List PassedCase
    , error : ( Maybe String, Maybe String )
    }


type SingleTaskStatus
    = SingleDone
    | SingleActive
    | SingleCancel


type SlideTaskStatus
    = SlideDone Date
    | SlideActive
    | SlideCancel Date


type CronTaskStatus
    = CronDone
    | CronCancel


type alias PassedCase =
    { date : Date
    , status : CronTaskStatus
    }


type TaskStatus
    = Active
    | Done
    | Cancel


getStartDate : TaskValue -> Date
getStartDate task =
    case task of
        Single t ->
            t.date

        Slide t ->
            t.startDate

        CronType t ->
            t.startDate


getValue : TaskValue -> String
getValue task =
    case task of
        Single t ->
            t.value

        Slide t ->
            t.value

        CronType t ->
            t.value


getErrors : TaskValue -> List String
getErrors task =
    case task of
        CronType cronTask ->
            case cronTask.error of
                ( Just err1, Just err2 ) ->
                    [ err1, err2 ]

                ( Nothing, Just err2 ) ->
                    [ err2 ]

                ( Just err1, Nothing ) ->
                    [ err1 ]

                ( Nothing, Nothing ) ->
                    []

        Slide slideTask ->
            case slideTask.error of
                Just err ->
                    List.singleton err

                Nothing ->
                    []

        Single _ ->
            []
