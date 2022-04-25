namespace Domain

open System

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open Utils

module Validation =
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
    { raw: string
      value: Result<decimal, string> }

    static member Create raw =
        { raw = raw
          value = Validation.validateHours raw }

    member this.Update raw =
        { this with
            raw = raw
            value = Validation.validateHours raw }

/// Comment that is validated from a raw string
type ValidatedComment =
    { raw: string
      value: Result<string, string> }

    static member Create raw =
        { raw = raw
          value = Validation.validateComment raw }

    member this.Update raw =
        { this with
            raw = raw
            value = Validation.validateComment raw }

/// A single unit of work
type WorkUnit =
    { hours: ValidatedHours
      comment: ValidatedComment }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment.raw
        && String.IsNullOrWhiteSpace this.hours.raw

/// A WorkUnit that only contains valid values
type ValidatedWorkUnit =
    { hours: decimal
      comment: string }

module WorkUnit =
    let encoder (workUnit : WorkUnit) =
        Encode.object [
            "hours", Encode.string workUnit.hours.raw
            "comment", Encode.string workUnit.comment.raw
        ]

    let decoder : Decoder<WorkUnit> =
        Decode.object (fun get ->
            { hours = get.Required.Field "hours" (Decode.map (fun str -> ValidatedHours.Create str) Decode.string)
              comment = get.Required.Field "comment" (Decode.map (fun str -> ValidatedComment.Create str) Decode.string) }
        )

    let validate (raw : WorkUnit) : ValidatedWorkUnit option =
        match raw.hours.value, raw.comment.value with
        | Ok hours, Ok comment -> Some { ValidatedWorkUnit.hours = hours; comment = comment }
        | _ -> None

/// Represents the work units of a project for a single day 
type DailyWorkLog =
    { projectId: int64
      projectName: string
      scheduledHours: decimal
      committedHoursOtherDays: decimal
      workUnits: WorkUnit list }

module DailyWorkLog =
    /// Computes the total hours spend on a project this month
    let totalProjectHours (project: DailyWorkLog) =
        project.workUnits
        |> List.choose (fun u -> u.hours.value |> Result.toOption)
        |> List.sum

    let encoder (dailyWorkLog: DailyWorkLog) : JsonValue =
        Encode.object [
            "projectName", Encode.string dailyWorkLog.projectName
            "projectId", Encode.int64 dailyWorkLog.projectId
            "scheduledHours", Encode.decimal dailyWorkLog.scheduledHours
            "committedHoursOtherDays", Encode.decimal dailyWorkLog.committedHoursOtherDays
            "workUnits", Encode.list (List.map WorkUnit.encoder dailyWorkLog.workUnits)
        ]

    let decoder: Decoder<DailyWorkLog> =
        Decode.object (fun get ->
            { projectId = get.Required.Field "projectId" Decode.int64
              projectName = get.Required.Field "projectName" Decode.string
              scheduledHours = get.Required.Field "scheduledHours" Decode.decimal
              committedHoursOtherDays = get.Required.Field "committedHoursOtherDays" Decode.decimal
              workUnits = get.Required.Field "workUnits" (Decode.list WorkUnit.decoder) }
        )
