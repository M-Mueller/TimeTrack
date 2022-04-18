module Domain

open System
open Utils

type User = { id: int64; name: string }

type ProjectName = string

/// A work log for a single day.
/// Use as DailyWorkLog or RawDailyWorkLog.
type GenericDailyWorkLog<'a> =
    { name: string
      scheduledHours: decimal
      committedHoursOtherDays: decimal
      workUnits: 'a list }

/// A single unit of work.
type WorkUnit =
    { hours: decimal
      comment: string }

/// A single unit of work as entered by the user, that hasn't been validated yet
type RawWorkUnit =
    { hours: string
      comment: string }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment
        && String.IsNullOrWhiteSpace this.hours

/// An intermediate type from a RawWorkUnit to a WorkUnit.
/// Contains the validation results including any potential error messages.
type ValidatedWorkUnit =
    { hours: Result<decimal, string>
      comment: Result<string, string> }

/// Checks whether all values of a RawWorkUnit are valid.
let validate (unit: RawWorkUnit) : ValidatedWorkUnit =
    { hours =
          unit.hours
          |> tryParseDecimal
          |> Result.fromOption "Please enter a number"
          |> Result.bind
              (fun hours ->
                  if hours <= 0.0m then
                      Error "Must be larger than 0"
                  elif hours > 24.0m then
                      Error "Must be smaller than 24"
                  else
                      Ok hours)
      comment = Ok unit.comment }

let toRawWorkUnit (unit: WorkUnit) : RawWorkUnit =
    { hours = string unit.hours
      comment = unit.comment }

type DailyWorkLog = GenericDailyWorkLog<WorkUnit> 
type RawDailyWorkLog = GenericDailyWorkLog<RawWorkUnit> 

let toRawDailyWorkLog (workLog : DailyWorkLog) : RawDailyWorkLog =
    { name = workLog.name
      scheduledHours = workLog.scheduledHours
      committedHoursOtherDays = workLog.committedHoursOtherDays
      workUnits = List.map toRawWorkUnit workLog.workUnits }

/// Computes the total hours spend on a project this month
let totalProjectHours (project: RawDailyWorkLog) =
    project.workUnits
    |> List.map validate
    |> List.choose (fun u -> u.hours |> Result.toOption)
    |> List.sum

/// Represents a single JIRA issue
type Issue = { key: string; title: string }
