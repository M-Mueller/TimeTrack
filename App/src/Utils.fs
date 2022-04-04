module Utils

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