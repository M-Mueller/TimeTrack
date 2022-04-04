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

type ValidatedWorkUnit =
    { hours: Result<float, string>
      comment: Result<string, string> }

let validate (unit: WorkUnit) : ValidatedWorkUnit =
    { hours =
          unit.hours
          |> tryParseFloat
          |> Result.fromOption "Please enter a number"
      comment = Ok unit.comment }

type WorkUnitInProject =
    { project: ProjectName
      index: int
      unit: WorkUnit }
