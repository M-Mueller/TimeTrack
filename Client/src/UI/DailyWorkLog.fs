/// Component that renders a list of DailyWorkLogs.
/// The user can create work units by entering hours and comments.
/// By default, only projects that already contain work units or
/// projects that have an assigned target are shown.
/// The user can add any other project from a list.
[<RequireQualifiedAccess>]
module UI.DailyWorkLog

open System
open Domain
open ClientDomain
open Feliz
open Utils

type State =
    {
      // Project select in the combobox that will be added when clicking Add
      selectedProject: ProjectName

      // Log entries for all possible projects. Individual workUnit might be empty.
      projects: RawDailyWorkLog list
      // Projects that are currently visible. Contains at least all projects with non-empty workUnits.
      activeProjects: Set<ProjectName> }

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
    | ChangeSelectedProject of ProjectName
    | ActivateSelectedProject
    | RemoveActiveProject of ProjectName
    | UpdateWorkUnit of WorkUnitInProject
    | AppendWorkUnit of WorkUnitInProject
    | RemoveLastWorkUnit of WorkUnitInProject


let init (projects: RawDailyWorkLog list) =
    let activeProjects =
        projects
        |> List.filter (fun p -> p.scheduledHours <> 0m || not p.workUnits.IsEmpty)
        |> List.map (fun p -> p.name)
        |> Set

    { selectedProject = defaultSelectedProject projects activeProjects
      projects = projects
      activeProjects = activeProjects }


let update (msg: Msg) (state: State) : State =
    match msg with
    | ChangeSelectedProject selected -> { state with selectedProject = selected }

    | ActivateSelectedProject ->
        if state.activeProjects.Contains state.selectedProject then
            state
        else
            let newActiveProjects = Set.add state.selectedProject state.activeProjects

            { state with
                activeProjects = newActiveProjects
                selectedProject = defaultSelectedProject state.projects newActiveProjects }

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
            selectedProject = newSelectedProject }

    | UpdateWorkUnit newWorkUnit ->
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = newWorkUnit.project then
                    { p with workUnits = List.updateAt newWorkUnit.index newWorkUnit.unit p.workUnits }
                else
                    p)

        { state with projects = newProjects }

    | AppendWorkUnit newWorkUnit ->
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = newWorkUnit.project then
                    { p with workUnits = p.workUnits @ [ newWorkUnit.unit ] }
                else
                    p)

        { state with projects = newProjects }

    | RemoveLastWorkUnit workUnit ->
        let newProjects =
            state.projects
            |> List.map (fun p ->
                if p.name = workUnit.project then
                    { p with workUnits = List.take (p.workUnits.Length - 1) p.workUnits }
                else
                    p)

        { state with projects = newProjects }


let renderAddProject (dispatch: Msg -> unit) (projects: ProjectName list) (selected: ProjectName) =
    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.row
            style.alignItems.center
            style.marginTop (length.rem 2)
            style.marginBottom (length.rem 2)
        ]
        prop.children [
            Html.select [
                prop.disabled projects.IsEmpty
                prop.style [
                    style.marginRight (length.rem 1)
                    style.width (length.percent 100)
                ]
                prop.value selected
                prop.children (
                    projects
                    |> List.map (fun p -> Html.option [ prop.text p ])
                )
                prop.onChange (ChangeSelectedProject >> dispatch)
            ]
            Html.button [
                prop.disabled (
                    projects.IsEmpty
                    || String.IsNullOrWhiteSpace selected
                )
                prop.text "Add"
                prop.onClick (fun _ -> dispatch ActivateSelectedProject)
            ]
        ]
    ]

let renderWorkUnit (dispatch: Msg -> unit) (maxIndex: int) (projectUnit: WorkUnitInProject) =
    let dispatchChange (newUnit: RawWorkUnit) =
        let msg =
            if projectUnit.index = -1 then
                AppendWorkUnit
            elif projectUnit.index = maxIndex && newUnit.isEmpty then
                RemoveLastWorkUnit
            else
                UpdateWorkUnit

        dispatch (msg { projectUnit with unit = newUnit })

    let hoursError =
        if projectUnit.index = -1 && projectUnit.unit.isEmpty then
            // The last row is empty by default
            ""
        else
            projectUnit.unit.hours.value
            |> Result.getError
            |> Option.defaultValue ""

    Html.div [
        prop.style [
            style.display.flex
            style.flexWrap.wrap
        ]
        prop.children [
            Elements.labeledInput
                "Hours"
                hoursError
                []
                [ prop.style [ style.maxWidth 200 ]
                  prop.type'.text
                  prop.inputMode.decimal
                  prop.value projectUnit.unit.hours.raw
                  prop.onChange (fun hours ->
                      dispatchChange
                          { projectUnit.unit with hours = projectUnit.unit.hours.Update hours }) ]
            Elements.labeledInput
                "Comment"
                ""
                [ prop.style [ style.flexGrow 1 ] ]
                [ prop.type'.text
                  prop.value projectUnit.unit.comment.raw
                  prop.onChange (fun comment ->
                      dispatchChange
                          { projectUnit.unit with comment = projectUnit.unit.comment.Update comment }) ]
        ]
    ]

let renderProject (dispatch: Msg -> unit) (project: RawDailyWorkLog) =
    let projectUnits =
        project.workUnits
        |> List.mapi (fun index unit ->
            { project = project.name
              index = index
              unit = unit })

    let maxIndex = List.length projectUnits - 1

    let totalHours = totalProjectHours project

    Html.article [
        prop.className "doodle-border"
        prop.children [
            yield
                (Html.header [
                    prop.style [
                        style.display.flex
                        style.flexDirection.row
                        style.alignItems.center
                    ]
                    prop.children [
                        Html.text project.name
                        Html.span [
                            prop.style [ style.flexGrow 1 ]
                        ]
                        Html.button [
                            prop.style [ style.color.red ]
                            prop.className [ "pseudo" ]
                            prop.text "X"
                            prop.onClick (fun _ -> dispatch (RemoveActiveProject project.name))
                        ]
                    ]
                 ])
            yield! (List.map (renderWorkUnit dispatch maxIndex) projectUnits)
            yield
                (renderWorkUnit
                    dispatch
                    maxIndex
                    { project = project.name
                      index = -1
                      unit =
                        { hours = ValidatedHours.Create ""
                          comment = ValidatedComment.Create "" } })
            yield
                Html.footer [
                    prop.style [
                        style.display.flex
                        style.flexDirection.row
                        style.alignItems.center
                    ]
                    prop.children [
                        Html.text $"Total Today: {totalHours} hours"
                        Html.span [
                            prop.style [ style.flexGrow 1 ]
                        ]
                        Html.text
                            $"This Month: {project.committedHoursOtherDays + totalHours} / {project.scheduledHours} scheduled"
                    ]
                ]
        ]
    ]


let render (dispatch: Msg -> unit) (state: State) =
    Html.div [
        prop.style [ style.flexGrow 1 ]
        prop.children [
            yield
                renderAddProject dispatch (selectableProjects state.projects state.activeProjects) state.selectedProject
            yield!
                (state.projects
                 |> List.filter (fun p -> state.activeProjects.Contains p.name)
                 |> List.sortBy (fun p -> p.name)
                 |> List.map (renderProject dispatch))
        ]
    ]
