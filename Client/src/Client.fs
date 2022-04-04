module Client

open System
open Elmish
open Elmish.React
open Feliz
open Utils
open Project

type State =
    { currentDate: System.DateTime
      scheduledHours: float

      selectedProject: ProjectName

      projects: Project list
      activeProjects: Set<ProjectName> }

let selectableProjects (projects: Project list) (activeProjects: Set<ProjectName>) : ProjectName list =
    projects
    |> List.map (fun p -> p.name)
    |> List.filter (fun n -> not (activeProjects.Contains n))

let defaultSelectedProject (allProjects: Project list) (activeProjects: Set<ProjectName>) : ProjectName =
    selectableProjects allProjects activeProjects
    |> List.tryHead
    |> Option.defaultValue ""

type Msg =
    | IncrementDate
    | DecrementDate
    | ChangeSelectedProject of ProjectName
    | ActivateSelectedProject
    | RemoveActiveProject of ProjectName
    | UpdateWorkUnit of WorkUnitInProject
    | AppendWorkUnit of WorkUnitInProject
    | RemoveLastWorkUnit of WorkUnitInProject

let init () =
    let projects =
        [ { name = "HR"
            scheduledHours = 42.0
            committedHoursOtherDays = 20.0
            workUnits =
                [ { WorkUnit.hours = "4.0"
                    comment = "Planning meeting" } ] }
          { name = "Company Admin"
            scheduledHours = 0.0
            committedHoursOtherDays = 5.0
            workUnits = [] }
          { name = "Suite"
            scheduledHours = 30.0
            committedHoursOtherDays = 15.0
            workUnits = [] } ]

    let activeProjects =
        projects
        |> List.filter (fun p -> not p.workUnits.IsEmpty)
        |> List.map (fun p -> p.name)
        |> Set

    { currentDate = DateTime.Now
      scheduledHours = 8
      selectedProject = defaultSelectedProject projects activeProjects
      projects = projects
      activeProjects = activeProjects }

let update (msg: Msg) (state: State) : State =
    match msg with
    | IncrementDate ->
        { state with
              currentDate = state.currentDate.AddDays(1) }

    | DecrementDate ->
        { state with
              currentDate = state.currentDate.AddDays(-1) }

    | ChangeSelectedProject selected ->
        { state with
              selectedProject = selected }

    | ActivateSelectedProject ->
        if state.activeProjects.Contains state.selectedProject then
            state
        else
            let newActiveProjects =
                Set.add state.selectedProject state.activeProjects

            { state with
                  activeProjects = newActiveProjects
                  selectedProject = defaultSelectedProject state.projects newActiveProjects }

    | RemoveActiveProject name ->
        let newActiveProjects = Set.remove name state.activeProjects

        // Clear workUnits of removed project
        let newProjects =
            state.projects
            |> List.map
                (fun p ->
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
            |> List.map
                (fun p ->
                    if p.name = newWorkUnit.project then
                        { p with
                              workUnits = List.updateAt newWorkUnit.index newWorkUnit.unit p.workUnits }
                    else
                        p)

        { state with projects = newProjects }

    | AppendWorkUnit (newWorkUnit) ->
        let newProjects =
            state.projects
            |> List.map
                (fun p ->
                    if p.name = newWorkUnit.project then
                        { p with
                              workUnits = p.workUnits @ [ newWorkUnit.unit ] }
                    else
                        p)

        { state with projects = newProjects }

    | RemoveLastWorkUnit workUnit ->
        let newProjects =
            state.projects
            |> List.map
                (fun p ->
                    if p.name = workUnit.project then
                        { p with
                              workUnits = List.take (p.workUnits.Length - 1) p.workUnits }
                    else
                        p)

        { state with projects = newProjects }

let renderDate (dispatch: Msg -> unit) (date: System.DateTime) =
    Html.div [
        prop.style [ style.display.flex ]
        prop.children [
            Html.button [
                prop.role "button"
                prop.classes [ "secondary; outline" ]
                prop.onClick (fun _ -> dispatch DecrementDate)
                prop.text "<"
            ]
            Html.h1 (date.ToString("d.MM.yyyy"))
            Html.button [
                prop.role "button"
                prop.classes [ "secondary; outline" ]
                prop.onClick (fun _ -> dispatch IncrementDate)
                prop.text ">"
            ]
        ]
    ]


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
    let dispatchChange (newUnit: WorkUnit) =
        let msg =
            if projectUnit.index = -1 then
                AppendWorkUnit
            elif projectUnit.index = maxIndex && newUnit.isEmpty then
                RemoveLastWorkUnit
            else
                UpdateWorkUnit

        dispatch (msg { projectUnit with unit = newUnit })

    let hoursError =
        if projectUnit.index = -1 then
            // The last row is empty by default
            ""
        else
            (validate projectUnit.unit).hours
            |> Result.getError
            |> Option.defaultValue ""

    Html.div [
        prop.style [ style.display.flex ]
        prop.children [
            Elements.labeledInput
                "Hours"
                hoursError
                []
                [ prop.style [ style.maxWidth 200 ]
                  prop.type'.number
                  prop.value projectUnit.unit.hours
                  prop.onChange (fun hours -> dispatchChange { projectUnit.unit with hours = hours }) ]
            Elements.labeledInput
                "Comment"
                ""
                [ prop.style [ style.flexGrow 1 ] ]
                [ prop.type'.text
                  prop.value projectUnit.unit.comment
                  prop.onChange
                      (fun comment ->
                          dispatchChange
                              { projectUnit.unit with
                                    comment = comment }) ]
        ]
    ]

let renderProject (dispatch: Msg -> unit) (project: Project) =
    let projectUnits =
        project.workUnits
        |> List.mapi
            (fun index unit ->
                { project = project.name
                  index = index
                  unit = unit })

    let maxIndex = List.length projectUnits - 1

    let totalHours = totalProjectHours project

    Html.article [
        prop.className "card"
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
            yield
                Html.header [
                    prop.style [
                        style.display.flex
                        style.flexDirection.row
                        style.alignItems.center
                    ]
                    prop.children [
                        Html.h5 $"Total: {totalHours} hours"
                        Html.span [
                            prop.style [ style.flexGrow 1 ]
                        ]
                        Html.h5 $"{project.committedHoursOtherDays + totalHours} / {project.scheduledHours} scheduled"
                    ]
                ]
            yield! (List.map (renderWorkUnit dispatch maxIndex) projectUnits)
            yield
                (renderWorkUnit
                    dispatch
                    maxIndex
                    { project = project.name
                      index = -1
                      unit = { hours = ""; comment = "" } })
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    let totalHoursToday =
        state.projects
        |> List.map totalProjectHours
        |> List.sum

    Html.div [
        yield renderDate dispatch state.currentDate
        yield
            Html.div [
                prop.style [
                    if totalHoursToday = 0 then
                        style.color.red
                    elif totalHoursToday < state.scheduledHours then
                        style.color.orange
                    elif totalHoursToday = state.scheduledHours then
                        style.color.black
                    else
                        style.color.green
                ]
                prop.text
                    $"Total hours: {totalHoursToday}/{state.scheduledHours} ({totalHoursToday - state.scheduledHours})"
            ]
        yield renderAddProject dispatch (selectableProjects state.projects state.activeProjects) state.selectedProject
        yield!
            (state.projects
             |> List.filter (fun p -> state.activeProjects.Contains p.name)
             |> List.sortBy (fun p -> p.name)
             |> List.map (renderProject dispatch))
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
