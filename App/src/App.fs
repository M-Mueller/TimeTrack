module App

open Elmish
open Elmish.React
open Feliz

type State = { count: int }

type Msg =
    | Increment
    | Decrement
    
let init () =
    { count = 0 }
    
let update (msg: Msg) (state: State): State =
    match msg with
    | Increment ->
        { state with count = state.count + 1 }

    | Decrement ->
        { state with count = state.count - 1 }

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch Increment)
      prop.text "Increment"
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch Decrement)
      prop.text "Decrement"
    ]

    Html.h1 state.count
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run