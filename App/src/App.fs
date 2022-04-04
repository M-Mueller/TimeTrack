module App

open System
open Elmish
open Elmish.React
open Feliz
open Utils
open Project

type State =
    { currentDate: System.DateTime
      selectedProject: ProjectName
      allProjects: ProjectName list
      projects: Map<ProjectName, WorkUnit list> }

let selectableProjects (allProjects: ProjectName list) (activeProjects: Map<ProjectName, WorkUnit list>) =
    allProjects
    |> List.filter (fun p -> not (activeProjects.ContainsKey p))

let defaultSelectedProject (allProjects: ProjectName list) (activeProjects: Map<ProjectName, WorkUnit list>) =
    selectableProjects allProjects activeProjects
    |> List.tryHead
    |> Option.defaultValue ""

type Msg =
    | IncrementDate
    | DecrementDate
    | ChangeSelectedProject of ProjectName
    | AddSelectedProject
    | RemoveProject of ProjectName
    | UpdateWorkUnit of WorkUnitInProject
    | AppendWorkUnit of WorkUnitInProject
    | RemoveWorkUnit of WorkUnitInProject

let init () =
    let allProjects = [ "HR"; "Company Admin"; "Suite" ]

    let projects =
        Map [
            ("HR",
             [ { WorkUnit.hours = "4.0"
                 comment = "Planning meeting" } ])
        ]

    { currentDate = DateTime.Now
      selectedProject = defaultSelectedProject allProjects projects
      allProjects = allProjects
      projects = projects }

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

    | AddSelectedProject ->
        if state.projects.ContainsKey state.selectedProject then
            state
        else
            let newProjects =
                state.projects.Add(state.selectedProject, [])

            { state with
                  projects = newProjects
                  selectedProject = defaultSelectedProject state.allProjects newProjects }

    | RemoveProject name ->
        let newProjects = state.projects.Remove name

        let newSelectedProject =
            if String.IsNullOrWhiteSpace state.selectedProject then
                defaultSelectedProject state.allProjects newProjects
            else
                state.selectedProject

        { state with
              projects = newProjects
              selectedProject = newSelectedProject }

    | UpdateWorkUnit newWorkUnit ->
        assert state.projects.ContainsKey newWorkUnit.project

        let oldWorkUnits = state.projects.Item newWorkUnit.project

        let newWorkUnits =
            List.updateAt newWorkUnit.index newWorkUnit.unit oldWorkUnits

        { state with
              projects = state.projects.Add(newWorkUnit.project, newWorkUnits) }

    | AppendWorkUnit (newWorkUnit) ->
        assert state.projects.ContainsKey newWorkUnit.project

        let newWorkUnits =
            (state.projects.Item newWorkUnit.project)
            @ [ newWorkUnit.unit ]

        { state with
              projects = state.projects.Add(newWorkUnit.project, newWorkUnits) }

    | RemoveWorkUnit workUnit ->
        assert state.projects.ContainsKey workUnit.project

        let oldWorkUnits = state.projects.Item workUnit.project

        let newWorkUnits =
            List.take (oldWorkUnits.Length - 1) oldWorkUnits

        { state with
              projects = state.projects.Add(workUnit.project, newWorkUnits) }

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
                prop.onClick (fun _ -> dispatch AddSelectedProject)
            ]
        ]
    ]

let renderWorkUnit (dispatch: Msg -> unit) (maxIndex: int) (projectUnit: WorkUnitInProject) =
    let dispatchChange (newUnit: WorkUnit) =
        let msg =
            if projectUnit.index = -1 then
                AppendWorkUnit
            elif projectUnit.index = maxIndex && newUnit.isEmpty then
                RemoveWorkUnit
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

let renderProject (dispatch: Msg -> unit) (name: string) (workUnits: WorkUnit list) =
    let projectUnits =
        workUnits
        |> List.mapi
            (fun index unit ->
                { project = name
                  index = index
                  unit = unit })

    let maxIndex = List.length workUnits - 1

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
                        Html.h5 name
                        Html.span [
                            prop.style [ style.flexGrow 1 ]
                        ]
                        Html.button [
                            prop.style [ style.color.red ]
                            prop.className [ "pseudo" ]
                            prop.text "X"
                            prop.onClick (fun _ -> dispatch (RemoveProject name))
                        ]
                    ]
                 ])
            yield! (List.map (renderWorkUnit dispatch maxIndex) projectUnits)
            yield
                (renderWorkUnit
                    dispatch
                    maxIndex
                    { project = name
                      index = -1
                      unit = { hours = ""; comment = "" } })
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        yield renderDate dispatch state.currentDate
        yield renderAddProject dispatch (selectableProjects state.allProjects state.projects) state.selectedProject
        yield!
            (state.projects
             |> Map.map (renderProject dispatch)
             |> Map.toList
             |> List.map snd)
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
