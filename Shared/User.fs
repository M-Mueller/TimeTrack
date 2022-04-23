module Domain.User

open System

type UserId = UserId of int64

type WorkingHours =
    { mon: int
      tue: int
      wed: int
      thu: int
      fri: int }

let workingHoursForDate (hours: WorkingHours) (date: DateTime) =
    match date.DayOfWeek with
    | DayOfWeek.Monday -> hours.mon
    | DayOfWeek.Tuesday -> hours.tue
    | DayOfWeek.Wednesday -> hours.wed
    | DayOfWeek.Thursday -> hours.thu
    | DayOfWeek.Friday -> hours.fri
    | _ -> 0


type User =
    { name: string
      workingHours: WorkingHours }
