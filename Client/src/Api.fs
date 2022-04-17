module Api

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


let getProjects (message: RemoteData<DailyWorkLog list> -> 'msg) : Cmd<'msg> =
    let url = "/api/v1/projects"

    async {
        let! (statusCode, responseText) = Http.get url

        let data =
            if statusCode = 200 then
                match Decode.Auto.fromString<DailyWorkLog list> (responseText, extra = (Extra.empty |> Extra.withDecimal)) with
                | Ok logs ->
                    printfn $"request: {statusCode}"
                    Success logs
                | Error error -> Failure $"Decoding failed with {error}"
            else
                Failure $"Requesting {url} failed with code {statusCode}"

        return message data
    }
    |> fromAsync
