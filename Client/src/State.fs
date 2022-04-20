module State

open System
open Fable.Core
open Elmish

open Domain

type State =
    { currentDate: DateTime
      // Total hours the user should work on this day
      scheduledHours: decimal

      user: Api.RemoteData<User>

      workLogState: Api.RemoteData<UI.DailyWorkLog.State>

      relatedIssues: Api.RemoteData<Issue list> }

type Msg =
    | IncrementDate
    | DecrementDate
    | DailyWorkLogMsg of UI.DailyWorkLog.Msg
    | WriteToClipboard of string
    | UserReceived of Api.RemoteData<User>
    | WorkLogReceived of Api.RemoteData<RawDailyWorkLog list>
    | RelatedIssuesReceived of Api.RemoteData<Issue list>

let changeDate (date: DateTime) state =
    { state with
        currentDate = date
        workLogState = Api.Loading
        relatedIssues = Api.Loading },
    Cmd.batch [
        Api.getDailyWorkLog date WorkLogReceived
        Api.getRelatedIssues RelatedIssuesReceived
    ]

let init () =
    let currentDate = DateTime.Now

    let state =
        { currentDate = DateTime()
          scheduledHours = 8m
          user = Api.Loading
          workLogState = Api.NotAsked
          relatedIssues = Api.NotAsked }

    let state, cmd = changeDate currentDate state

    (state,
     Cmd.batch [
         cmd
         Api.getUser UserReceived
     ])

[<Emit("navigator.clipboard.writeText($0)")>]
let private writeToClipboard (text: string) : unit = jsNative

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | IncrementDate -> changeDate (state.currentDate.AddDays(1)) state

    | DecrementDate -> changeDate (state.currentDate.AddDays(-1)) state

    | DailyWorkLogMsg msg' ->
        match state.workLogState with
        | Api.Success state' -> { state with workLogState = Api.Success(UI.DailyWorkLog.update msg' state') }, Cmd.none
        | _ -> state, Cmd.none

    | WriteToClipboard text ->
        writeToClipboard text
        state, Cmd.none

    | UserReceived user -> { state with user = user }, Cmd.none

    | WorkLogReceived data ->
        { state with
            workLogState =
                match data with
                | Api.NotAsked -> Api.NotAsked
                | Api.Loading -> Api.Loading
                | Api.Failure error -> Api.Failure error
                | Api.Success workLog -> Api.Success(UI.DailyWorkLog.init workLog) },
        Cmd.none

    | RelatedIssuesReceived issues -> { state with relatedIssues = issues }, Cmd.none
