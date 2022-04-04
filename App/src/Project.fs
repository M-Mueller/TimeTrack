module Project

open System

type ProjectName = string

type WorkUnit =
    { hours: string
      comment: string }
    member this.isEmpty =
        String.IsNullOrWhiteSpace this.comment
        && String.IsNullOrWhiteSpace this.hours

type WorkUnitInProject =
    { project: ProjectName
      index: int
      unit: WorkUnit }
