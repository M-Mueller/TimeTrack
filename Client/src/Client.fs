module Client

open System
open Fable.Core
open Elmish
open Elmish.React
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

let renderTotalHours (projects: RawDailyWorkLog list) (scheduledHours: decimal) =
    let totalHoursToday = projects |> List.map totalProjectHours |> List.sum

    Html.div [
        prop.style [
            style.marginTop 10
            if totalHoursToday = 0m then
                style.color.red
            elif totalHoursToday < scheduledHours then
                style.color.orange
            elif totalHoursToday = scheduledHours then
                style.color.black
            else
                style.color.green
        ]
        prop.text $"Total hours: {totalHoursToday}/{scheduledHours} ({totalHoursToday - scheduledHours})"
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

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        renderDate dispatch state.currentDate
        Html.div [
            prop.style [
                style.display.flex
                style.flexDirection.row
            ]
            prop.children [
                Html.div [
                    prop.style [ style.flexGrow 1 ]
                    prop.children [
                        match state.workLogState with
                        | Api.NotAsked -> Html.text "Please reload the page"
                        | Api.Loading -> Html.text "Loading..."
                        | Api.Failure error -> Html.text $"Could not load work log: {error}"
                        | Api.Success projects -> UI.DailyWorkLog.renderProjects (DailyWorkLogMsg >> dispatch) projects
                    ]
                ]
                renderRelatedIssues dispatch state.relatedIssues
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
