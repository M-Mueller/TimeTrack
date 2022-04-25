module Api

open System
open Fable.SimpleHttp
open Elmish
open Thoth.Json

open Domain

type RemoteData<'value> =
    | NotAsked
    | Loading
    | Failure of string
    | Success of 'value

module RemoteData =
    let map (f: 'a -> 'b) (d: RemoteData<'a>) : RemoteData<'b> =
        match d with
        | NotAsked -> NotAsked
        | Loading -> Loading
        | Failure m -> Failure m
        | Success v -> Success(f v)


let internal fromAsync (operation: Async<'msg>) : Cmd<'msg> =
    let delayedCmd (dispatch: 'msg -> unit) : unit =
        let delayedDispatch =
            async {
                let! msg = operation
                dispatch msg
            }

        Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd

let inline getRemoteData url (decoder: Decoder<'data>) (message: RemoteData<'data> -> 'msg) : Cmd<'msg> =
    async {
        let! (statusCode, responseText) = Http.get url

        let data =
            if statusCode = 200 then
                let decoded = Decode.fromString decoder responseText

                match decoded with
                | Ok user -> Success user
                | Error error -> Failure $"Decoding failed with {error}"
            else
                Failure $"Requesting {url} failed with code {statusCode}"

        return message data
    }
    |> fromAsync


let getUser (message: RemoteData<User> -> 'msg) : Cmd<'msg> = getRemoteData "/api/v1/user" User.decoder message

let getDailyWorkLog (date: DateTime) (message: RemoteData<DailyWorkLog list> -> 'msg) : Cmd<'msg> =
    let url = $"""/api/v1/dailyworklog/{date.ToString "yyyy-MM-dd"}"""

    getRemoteData url (Decode.list DailyWorkLog.decoder) message

let postDailyWorkLog (date: DateTime) (workLogs : DailyWorkLog list) (message: bool -> 'msg) : Cmd<'msg> =
    let url = $"""/api/v1/dailyworklog/{date.ToString "yyyy-MM-dd"}"""
    let body = 
        workLogs
        |> List.map DailyWorkLog.encoder
        |> Encode.list
        |> Encode.toString 4

    async {
        let! (statusCode, responseText) = Http.post url body
        return message (statusCode = 200)
    }
    |> fromAsync

let getRelatedIssues (message: RemoteData<Issue list> -> 'msg) : Cmd<'msg> =
    getRemoteData "/api/v1/relatedIssues" (Decode.list Issue.decoder) message
