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

module List =
    let updateAt index value =
        List.mapi (fun i old -> if i = index then value else old)
