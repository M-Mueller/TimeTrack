module Domain.RawDailyWorkLog

open System
open Utils
open Domain.DailyWorkLog

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

/// Hours that are validated from a raw string
type ValidatedHours =
    { raw : string
      value : Result<decimal, string> }

    static member Create raw =
        { raw = raw; value = validateHours raw }

    member this.Update raw =
        { this with raw = raw; value = validateHours raw }

/// Comment that is validated from a raw string
type ValidatedComment =
    { raw : string
      value : Result<string, string> }

    static member Create raw =
        { raw = raw; value = validateComment raw }

    member this.Update raw =
        { this with raw = raw; value = validateComment raw }

/// A single unit of work as entered by the user, that hasn't been validated yet
type RawWorkUnit =
    { hours: ValidatedHours
      comment: ValidatedComment }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment.raw
        && String.IsNullOrWhiteSpace this.hours.raw

/// Similar to DailyWorkLog, but the work units also contain the raw user input.
type RawDailyWorkLog = GenericDailyWorkLog<RawWorkUnit>

let toRawWorkUnit (unit: WorkUnit) : RawWorkUnit =
    { hours = ValidatedHours.Create (string unit.hours)
      comment = ValidatedComment.Create (string unit.comment) }

let toRawDailyWorkLog (workLog: DailyWorkLog) : RawDailyWorkLog =
    { name = workLog.name
      scheduledHours = workLog.scheduledHours
      committedHoursOtherDays = workLog.committedHoursOtherDays
      workUnits = List.map toRawWorkUnit workLog.workUnits }

/// Computes the total hours spend on a project this month
let totalProjectHours (project: RawDailyWorkLog) =
    project.workUnits
    |> List.choose (fun u -> u.hours.value |> Result.toOption)
    |> List.sum
