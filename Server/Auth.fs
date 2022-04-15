module Auth

open System
open Falco

/// Calls handleOk with the authenticated user or handleError if the authentication failed.
/// Uses the HTTP Basic Authentication scheme.
/// The authenticateUser receives the username and password from the Authorization header
/// and should return whether the user could be authenticated or not.
let ifAuthenticated
    (authenticateUser: string -> string -> Async<'a option>)
    (handleOk: 'a -> HttpHandler)
    (handleError: HttpHandler)
    : HttpHandler =
    fun ctx ->
        let splitCredentials (header: string) =
            match header.Split(' ', 2, StringSplitOptions.TrimEntries) with
            | [| "Basic"; credentials |] -> Some credentials
            | _ -> None

        let parseCredentials credentials =
            try
                Convert.FromBase64String credentials
                |> Text.Encoding.UTF8.GetString
                |> (fun s ->
                    match s.Split(':', 2) with
                    | [| username; password |] -> Some(username, password)
                    | _ -> None)
            with
            | _ -> None

        let headers = Request.getHeaders ctx

        headers.TryGetValue "Authorization"
        |> Option.bind Array.tryHead
        |> Option.bind splitCredentials
        |> Option.bind parseCredentials
        |> function
            | Some (username, password) ->
                task {
                    let! user = authenticateUser username password

                    return!
                        match user with
                        | Some user -> handleOk user ctx
                        | None -> handleError ctx
                }
            | None -> handleError ctx
