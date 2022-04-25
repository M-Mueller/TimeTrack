module Utils

open System

let tryParseInt (input: string) : Option<int> =
    try
        Some(int input)
    with
    | _ -> None

let tryParseFloat (input: string) : Option<float> =
    try
        Some(float input)
    with
    | _ -> None

let tryParseDecimal (input: string) : Option<decimal> =
    try
        Some(decimal input)
    with
    | _ -> None

module Result =
    let fromOption error option =
        match option with
        | Some value -> Ok value
        | None -> Error error

    let toOption option =
        match option with
        | Ok value -> Some value
        | Error _ -> None

    let getOk result =
        match result with
        | Ok value -> Some value
        | Error error -> None

    let getError result =
        match result with
        | Ok _ -> None
        | Error error -> Some error

    let defaultValue default_ result =
        match result with
        | Ok value -> value
        | Error _ -> default_

module List =
    let flattenOption (input: List<'a option>) : List<'a> option =
        List.foldBack
            (fun (value: 'a option) (state: List<'a> option) ->
                match state, value with
                | Some state, Some value -> Some(value :: state)
                | _ -> None)
            input
            (Some [])


module Async =
    let map f computation =
        async.Bind(computation, f >> async.Return)
