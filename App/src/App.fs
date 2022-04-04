module App

open System
open Elmish
open Elmish.React
open Feliz
open Utils
open Project

type State =
    { currentDate: System.DateTime
      allProjects: ProjectName list
      projects: Map<ProjectName, WorkUnit list> }

type Msg =
    | IncrementDate
    | DecrementDate
    | UpdateWorkUnit of WorkUnitInProject
    | AppendWorkUnit of WorkUnitInProject
    | RemoveWorkUnit of WorkUnitInProject

let init () =
    { currentDate = DateTime.Now
      allProjects = [ "HR"; "Company Admin"; "Suite" ]
      projects =
          Map [
              ("HR",
               [ { hours = "4.0"
                   comment = "Planning meeting" } ])
          ] }

let update (msg: Msg) (state: State) : State =
    match msg with
    | IncrementDate ->
        { state with
              currentDate = state.currentDate.AddDays(1) }
    | DecrementDate ->
        { state with
              currentDate = state.currentDate.AddDays(-1) }

    | UpdateWorkUnit (newWorkUnit) ->
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
            yield (Html.header [ Html.h5 name ])
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
    Html.div (
        Html.div [
            prop.style [ style.display.flex ]
            prop.children [
                Html.button [
                    prop.role "button"
                    prop.classes [ "secondary; outline" ]
                    prop.onClick (fun _ -> dispatch DecrementDate)
                    prop.text "<"
                ]
                Html.h1 (state.currentDate.ToString("d.MM.yyyy"))
                Html.button [
                    prop.role "button"
                    prop.classes [ "secondary; outline" ]
                    prop.onClick (fun _ -> dispatch IncrementDate)
                    prop.text ">"
                ]
            ]
        ]
        :: (state.projects
            |> Map.map (renderProject dispatch)
            |> Map.toList
            |> List.map snd)
    )

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
