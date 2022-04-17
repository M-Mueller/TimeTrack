module State

open System
open Fable.Core
open Elmish

open Domain

type State =
    { currentDate: DateTime
      // Total hours the user should work on this day
      scheduledHours: decimal

      // Project select in the combobox that will be added when clicking Add
      selectedProject: ProjectName

      // Log entries for all possible projects. Individual workUnit might be empty.
      projects: RawDailyWorkLog list
      // Projects that are currently visible. Contains at least all projects with non-empty workUnits.
      activeProjects: Set<ProjectName>

      relatedIssues: Issue list }

let selectableProjects (projects: RawDailyWorkLog list) (activeProjects: Set<ProjectName>) : ProjectName list =
    projects
    |> List.map (fun p -> p.name)
    |> List.filter (fun n -> not (activeProjects.Contains n))

let private defaultSelectedProject
    (allProjects: RawDailyWorkLog list)
    (activeProjects: Set<ProjectName>)
    : ProjectName =
    selectableProjects allProjects activeProjects
    |> List.tryHead
    |> Option.defaultValue ""


/// Represents a single WorkUnit in the state.projects list
type WorkUnitInProject =
    { project: ProjectName
      index: int
      unit: RawWorkUnit }

type Msg =
    | IncrementDate
    | DecrementDate
    | ChangeSelectedProject of ProjectName
    | ActivateSelectedProject
    | RemoveActiveProject of ProjectName
    | UpdateWorkUnit of WorkUnitInProject
    | AppendWorkUnit of WorkUnitInProject
    | RemoveLastWorkUnit of WorkUnitInProject
    | WriteToClipboard of string
    | DataReceived of Api.RemoteData<DailyWorkLog list>

let init () =
    let projects =
        [ { name = "HR"
            scheduledHours = 42.0m
            committedHoursOtherDays = 20.0m
            workUnits =
              [ { RawWorkUnit.hours = "4.0"
                  comment = "Planning meeting" } ] }
          { name = "Admin"
            scheduledHours = 0.0m
            committedHoursOtherDays = 5.0m
            workUnits = [] }
          { name = "Super Duper App"
            scheduledHours = 30.0m
            committedHoursOtherDays = 15.0m
            workUnits = [] } ]

    let activeProjects =
        projects
        |> List.filter (fun p -> not p.workUnits.IsEmpty)
        |> List.map (fun p -> p.name)
        |> Set

    { currentDate = DateTime.Now
      scheduledHours = 8m
      selectedProject = defaultSelectedProject projects activeProjects
      projects = projects
      activeProjects = activeProjects
      relatedIssues =
        [ { key = "HR-5"
            title = "Interview Bob" }
          { key = "SDA-51"
            title = "Crashes randomly" }
          { key = "SDA-42"
            title = "Moar Features!" } ] },
    Cmd.none

[<Emit("navigator.clipboard.writeText($0)")>]
let private writeToClipboard (text: string) : unit = jsNative

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | IncrementDate -> { state with currentDate = state.currentDate.AddDays(1) }, Cmd.none

    | DecrementDate -> { state with currentDate = state.currentDate.AddDays(-1) }, Cmd.none

    | ChangeSelectedProject selected -> { state with selectedProject = selected }, Cmd.none

    | ActivateSelectedProject ->
        if state.activeProjects.Contains state.selectedProject then
            state, Cmd.none
        else
            let newActiveProjects = Set.add state.selectedProject state.activeProjects

            { state with
                activeProjects = newActiveProjects
                selectedProject = defaultSelectedProject state.projects newActiveProjects },
            Cmd.none

    | RemoveActiveProject name ->
        let newActiveProjects = Set.remove name state.activeProjects

        // Clear workUnits of removed project
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = name then
                    { p with workUnits = [] }
                else
                    p)

        // Reset selected if it was empty
        let newSelectedProject =
            if String.IsNullOrWhiteSpace state.selectedProject then
                defaultSelectedProject state.projects newActiveProjects
            else
                state.selectedProject

        { state with
            projects = newProjects
            activeProjects = newActiveProjects
            selectedProject = newSelectedProject },
        Cmd.none

    | UpdateWorkUnit newWorkUnit ->
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = newWorkUnit.project then
                    { p with workUnits = List.updateAt newWorkUnit.index newWorkUnit.unit p.workUnits }
                else
                    p)

        { state with projects = newProjects }, Cmd.none

    | AppendWorkUnit newWorkUnit ->
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = newWorkUnit.project then
                    { p with workUnits = p.workUnits @ [ newWorkUnit.unit ] }
                else
                    p)

        { state with projects = newProjects }, Cmd.none

    | RemoveLastWorkUnit workUnit ->
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = workUnit.project then
                    { p with workUnits = List.take (p.workUnits.Length - 1) p.workUnits }
                else
                    p)

        { state with projects = newProjects }, Cmd.none

    | WriteToClipboard text ->
        writeToClipboard text
        state, Cmd.none

    | DataReceived data ->
        printfn $"Got {data}"
        state, Cmd.none
