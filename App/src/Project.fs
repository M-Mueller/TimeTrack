module Project

open System
open Utils

type ProjectName = string

type WorkUnit =
    { hours: string
      comment: string }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment
        && String.IsNullOrWhiteSpace this.hours

type Project =
    { name: string
      scheduledHours: float
      committedHoursOtherDays: float
      workUnits: WorkUnit list }

type ValidatedWorkUnit =
    { hours: Result<float, string>
      comment: Result<string, string> }

let validate (unit: WorkUnit) : ValidatedWorkUnit =
    { hours =
          unit.hours
          |> tryParseFloat
          |> Result.fromOption "Please enter a number"
      comment = Ok unit.comment }

let totalProjectHours (project: Project) =
    project.workUnits
    |> List.map validate
    |> List.choose (fun u -> u.hours |> Result.toOption)
    |> List.sum

type WorkUnitInProject =
    { project: ProjectName
      index: int
      unit: WorkUnit }
