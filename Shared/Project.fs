module Project

open System
open Utils

type User = { id: int64; email: string }

type Project = string

type ProjectName = string

type WorkUnit =
    { hours: string
      comment: string }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment
        && String.IsNullOrWhiteSpace this.hours

type ScheduledProject =
    { name: string
      scheduledHours: decimal
      committedHoursOtherDays: decimal
      workUnits: WorkUnit list }

type ValidatedWorkUnit =
    { hours: Result<decimal, string>
      comment: Result<string, string> }

let validate (unit: WorkUnit) : ValidatedWorkUnit =
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

let totalProjectHours (project: ScheduledProject) =
    project.workUnits
    |> List.map validate
    |> List.choose (fun u -> u.hours |> Result.toOption)
    |> List.sum

type WorkUnitInProject =
    { project: ProjectName
      index: int
      unit: WorkUnit }
