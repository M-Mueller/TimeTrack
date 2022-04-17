module Client

open System
open Fable.Core
open Elmish
open Elmish.React
open Feliz

open Utils
open Domain
open State

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        UI.renderDate dispatch state.currentDate
        Html.div [
            prop.style [
                style.display.flex
                style.flexDirection.row
            ]
            prop.children [
                UI.renderProjects dispatch state
                UI.renderRelatedIssues dispatch state.relatedIssues
            ]
        ]
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
