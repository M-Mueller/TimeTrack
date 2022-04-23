module Domain

open System
open Utils

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

type ProjectName = string

/// A work log for a single day.
/// Use as DailyWorkLog or RawDailyWorkLog.
type GenericDailyWorkLog<'a> =
    { name: string
      scheduledHours: decimal
      committedHoursOtherDays: decimal
      workUnits: 'a list }

/// A single unit of work.
type WorkUnit = { hours: decimal; comment: string }

/// The work log of a single project on a single day.
type DailyWorkLog = GenericDailyWorkLog<WorkUnit>

/// Validates a raw hours string
let validateHours (hours: string) : Result<decimal, string> =
    hours
    |> tryParseDecimal
    |> Result.fromOption "Please enter a number"
    |> Result.bind (fun hours ->
        if hours <= 0.0m then
            Error "Must be larger than 0"
        elif hours > 24.0m then
            Error "Must be smaller than 24"
        else
            Ok hours)

/// Validates a raw comment string
let validateComment (comment: string) : Result<string, string> =
    let maxLength = 200
    if comment.Length <= maxLength then
        Ok comment
    else
        Error $"Comment must be shorter than {maxLength} characters"

/// Represents a single JIRA issue
type Issue = { key: string; title: string }
