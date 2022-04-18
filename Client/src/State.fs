module State

open System
open Fable.Core
open Elmish

open Domain

type State =
    { currentDate: DateTime
      // Total hours the user should work on this day
      scheduledHours: decimal

      workLogState: Api.RemoteData<UI.DailyWorkLog.State>

      relatedIssues: Issue list }

let selectableProjects (projects: RawDailyWorkLog list) (activeProjects: Set<ProjectName>) : ProjectName list =
    projects
    |> List.map (fun p -> p.name)
    |> List.filter (fun n -> not (activeProjects.Contains n))

type Msg =
    | IncrementDate
    | DecrementDate
    | DailyWorkLogMsg of UI.DailyWorkLog.Msg
    | WriteToClipboard of string
    | WorkLogReceived of Api.RemoteData<RawDailyWorkLog list>

let init () =
    { currentDate = DateTime.Now
      scheduledHours = 8m
      workLogState = Api.Loading
      relatedIssues =
        [ { key = "HR-5"
            title = "Interview Bob" }
          { key = "SDA-51"
            title = "Crashes randomly" }
          { key = "SDA-42"
            title = "Moar Features!" } ] },
    Api.getProjects WorkLogReceived

[<Emit("navigator.clipboard.writeText($0)")>]
let private writeToClipboard (text: string) : unit = jsNative

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | IncrementDate -> { state with currentDate = state.currentDate.AddDays(1) }, Cmd.none

    | DecrementDate -> { state with currentDate = state.currentDate.AddDays(-1) }, Cmd.none

    | DailyWorkLogMsg msg' ->
        match state.workLogState with
        | Api.Success state' -> { state with workLogState = Api.Success(UI.DailyWorkLog.update msg' state') }, Cmd.none
        | _ -> state, Cmd.none

    | WriteToClipboard text ->
        writeToClipboard text
        state, Cmd.none

    | WorkLogReceived data ->
        { state with
            workLogState =
                match data with
                | Api.NotAsked -> Api.NotAsked
                | Api.Loading -> Api.Loading
                | Api.Failure error -> Api.Failure error
                | Api.Success workLog -> Api.Success(UI.DailyWorkLog.init workLog) },
        Cmd.none
