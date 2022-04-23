module Client

open System
open Fable.Core
open Elmish
open Elmish.React
open Feliz

open Utils
open Domain.User
open Domain.RawDailyWorkLog
open Domain.Misc
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

let renderTotalHours (projects: RawDailyWorkLog list) state =
    match state.user with
    | Api.Success user ->
        let totalHoursToday = projects |> List.map totalProjectHours |> List.sum

        let workingHours = decimal (workingHoursForDate user.workingHours state.currentDate)

        Html.div [
            prop.style [
                style.marginTop 10
                if totalHoursToday = 0m then
                    style.color.red
                elif totalHoursToday < workingHours then
                    style.color.orange
                elif totalHoursToday = workingHours then
                    style.color.black
                else
                    style.color.green
            ]
            prop.text $"Total Today: {totalHoursToday}/{workingHours} ({totalHoursToday - workingHours})"
        ]
    | Api.Failure error -> Html.text $"Could not load user info: {error}"
    | _ -> Html.div []

let renderButtons dispatch =
    Html.div [
        prop.style [
            style.display.flex
            style.width (length.percent 100)
            style.justifyContent.spaceBetween
        ]
        prop.children [
            Html.button [
                prop.onClick (fun _ -> dispatch ResetDailyWorkLog)
                prop.children [ Html.text "Reset" ]
            ]
            Html.button [
                prop.onClick (fun _ -> dispatch CommitDailyWorkLog)
                prop.children [ Html.text "Commit" ]
            ]
        ]
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
                        | Api.Success projects ->
                            renderTotalHours projects.projects state
                            UI.DailyWorkLog.render (DailyWorkLogMsg >> dispatch) projects
                            renderButtons dispatch
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
