module ClientDomain

open System
open Domain
open Utils

/// A value that needs to be validated from a raw user input.
type ValidatedValue<'a> =
    { raw: string
      value: Result<'a, string>
      validator: string -> Result<'a, string> }

module ValidatedValue =
    let create validator initial =
        { raw = initial
          value = validator initial
          validator = validator }

    let update vv raw =
        { vv with
            raw = raw
            value = vv.validator raw }

/// A single unit of work as entered by the user, that hasn't been validated yet
type RawWorkUnit =
    { hours: ValidatedValue<decimal>
      comment: ValidatedValue<string> }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment.raw
        && String.IsNullOrWhiteSpace this.hours.raw

/// Similar to DailyWorkLog, but the work units also contain the raw user input.
type RawDailyWorkLog = GenericDailyWorkLog<RawWorkUnit>

let toRawWorkUnit (unit: WorkUnit) : RawWorkUnit =
    { hours = ValidatedValue.create validateHours (string unit.hours)
      comment = ValidatedValue.create validateComment (string unit.comment) }

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
