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
                        | Api.Success projects -> UI.DailyWorkLog.render (DailyWorkLogMsg >> dispatch) projects
                    ]
                ]
                (match state.relatedIssues with
                 | Api.NotAsked -> Html.text "Please reload the page"
                 | Api.Loading -> Html.text "Loading..."
                 | Api.Failure error -> Html.text $"Could not load issues: {error}"
                 | Api.Success issues -> UI.RelatedIssues.render (fun text -> dispatch (WriteToClipboard text)) issues)
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
