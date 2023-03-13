module Model exposing (..)

import Cron exposing (Cron)
import Date exposing (Date)
import DatePicker as DT


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


type alias TaskValue =
    { value : String
    , createdDate : Date
    , editDate : Date
    , date : Date
    , taskType : TaskType
    , error : List String
    }


type TaskType
    = Single TaskStatus
    | CronType CronTaskValue
    | Slide SlideTaskValue


type alias SlideTaskValue =
    { endDate : Date
    , status : TaskStatus
    }


type alias CronTaskValue =
    { cron : Cron
    , cronEditValue : String
    , endDate : Date
    , cases : List PassedCase
    }


type TaskStatus
    = Done
    | Active
    | Cancel


type alias PassedCase =
    { date : Date
    , status : TaskStatus
    }
