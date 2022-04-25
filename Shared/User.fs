namespace Domain

open System

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type UserId = UserId of int64

type WorkingHours =
    { mon: int
      tue: int
      wed: int
      thu: int
      fri: int }

type User =
    { name: string
      workingHours: WorkingHours }

module WorkingHours =
    let decoder: Decoder<WorkingHours> =
        Decode.object (fun get ->
            { mon = get.Required.Field "mon" Decode.int
              tue = get.Required.Field "tue" Decode.int
              wed = get.Required.Field "wed" Decode.int
              thu = get.Required.Field "thu" Decode.int
              fri = get.Required.Field "fri" Decode.int })

module User =
    let workingHoursForDate (hours: WorkingHours) (date: DateTime) =
        match date.DayOfWeek with
        | DayOfWeek.Monday -> hours.mon
        | DayOfWeek.Tuesday -> hours.tue
        | DayOfWeek.Wednesday -> hours.wed
        | DayOfWeek.Thursday -> hours.thu
        | DayOfWeek.Friday -> hours.fri
        | _ -> 0

    let decoder: Decoder<User> =
        Decode.object (fun get ->
            { name = get.Required.Field "name" Decode.string
              workingHours = get.Required.Field "workingHours" WorkingHours.decoder })
