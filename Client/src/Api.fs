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


let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
    let delayedCmd (dispatch: 'msg -> unit) : unit =
        let delayedDispatch =
            async {
                let! msg = operation
                dispatch msg
            }

        Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd


let getUser (message: RemoteData<User> -> 'msg) : Cmd<'msg> =
    let url = "/api/v1/user"

    async {
        let! (statusCode, responseText) = Http.get url

        let data =
            if statusCode = 200 then
                let decoded = 
                    Decode.Auto.fromString<User>
                        (
                            responseText,
                            extra = (Extra.empty |> Extra.withDecimal)
                        )
                match decoded with
                | Ok user -> Success user
                | Error error -> Failure $"Decoding failed with {error}"
            else
                Failure $"Requesting {url} failed with code {statusCode}"

        return message data
    }
    |> fromAsync


let getDailyWorkLog (date: DateTime) (message: RemoteData<RawDailyWorkLog list> -> 'msg) : Cmd<'msg> =
    let url = $"""/api/v1/dailyworklog/{date.ToString "yyyy-MM-dd"}"""

    async {
        let! (statusCode, responseText) = Http.get url

        let data =
            if statusCode = 200 then
                let decoded = 
                    Decode.Auto.fromString<DailyWorkLog list>
                        (
                            responseText,
                            extra = (Extra.empty |> Extra.withDecimal)
                        )
                match decoded with
                | Ok logs -> Success (List.map toRawDailyWorkLog logs)
                | Error error -> Failure $"Decoding failed with {error}"
            else
                Failure $"Requesting {url} failed with code {statusCode}"

        return message data
    }
    |> fromAsync

let getRelatedIssues (message: RemoteData<Issue list> -> 'msg) : Cmd<'msg> =
    let url = "/api/v1/relatedIssues"

    async {
        let! (statusCode, responseText) = Http.get url

        let data =
            if statusCode = 200 then
                let decoded = 
                    Decode.Auto.fromString<Issue list>(responseText)
                match decoded with
                | Ok issues -> Success issues
                | Error error -> Failure $"Decoding failed with {error}"
            else
                Failure $"Requesting {url} failed with code {statusCode}"

        return message data
    }
    |> fromAsync
