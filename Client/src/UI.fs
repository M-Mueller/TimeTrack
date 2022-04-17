module UI

open System
open Elmish
open Feliz

open Utils
open Domain
open State

let renderDate (dispatch: Msg -> unit) (date: DateTime) =
    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.row
            style.alignItems.center
        ]
        prop.children [
            Html.button [
                prop.role "button"
                prop.classes [ "secondary; outline" ]
                prop.onClick (fun _ -> dispatch DecrementDate)
                prop.text "<"
            ]
            Html.span [
                prop.style [ style.margin (0, 5) ]
                prop.text (date.ToString("d.MM.yyyy"))
            ]
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
        if projectUnit.index = -1 then
            // The last row is empty by default
            ""
        else
            (validate projectUnit.unit).hours
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
                  prop.type'.number
                  prop.value projectUnit.unit.hours
                  prop.onChange (fun hours -> dispatchChange { projectUnit.unit with hours = hours }) ]
            Elements.labeledInput
                "Comment"
                ""
                [ prop.style [ style.flexGrow 1 ] ]
                [ prop.type'.text
                  prop.value projectUnit.unit.comment
                  prop.onChange (fun comment -> dispatchChange { projectUnit.unit with comment = comment }) ]
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

let renderProjects (dispatch: Msg -> unit) (state: State) =
    let totalHoursToday =
        state.projects
        |> List.map totalProjectHours
        |> List.sum

    Html.div [
        prop.style [ style.flexGrow 1 ]
        prop.children [
            yield
                Html.div [
                    prop.style [
                        style.marginTop 10
                        if totalHoursToday = 0m then
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
            yield
                renderAddProject dispatch (selectableProjects state.projects state.activeProjects) state.selectedProject
            yield!
                (state.projects
                 |> List.filter (fun p -> state.activeProjects.Contains p.name)
                 |> List.sortBy (fun p -> p.name)
                 |> List.map (renderProject dispatch))
        ]
    ]

let renderRelatedIssues (dispatch: Msg -> unit) (issues: Issue list) =
    let renderIssue issue =
        Html.div [
            prop.className "doodle-border"
            prop.style [
                style.margin 6
                style.position.relative
            ]
            prop.children [
                Html.a [
                    prop.href $"http://example.com/issues/{issue.key}"
                    prop.text issue.key
                    prop.style [
                        style.display.block
                        style.fontSize (length.pt 16)
                    ]
                ]
                Html.a [
                    prop.style [
                        style.cursor.pointer
                        style.position.absolute
                        style.right 2
                        style.top 2
                    ]
                    prop.text "📋"
                    prop.title "Copy to clipboard"
                    prop.onClick (fun _ ->
                        WriteToClipboard $"[{issue.key}] {issue.title}"
                        |> dispatch)

                    ]
                Html.text issue.title
            ]
        ]

    Html.div [
        prop.className "doodle-border"
        prop.style [
            style.maxWidth (length.px 300)
            style.marginLeft 20
        ]
        prop.children (
            Html.text "Related JIRA Issues"
            :: List.map renderIssue issues
        )
    ]
