module ApiHandler

open System
open Falco
open Thoth.Json.Net

open Domain
open Utils

let getUser db (userid: UserId) : HttpHandler =
    fun ctx ->
        task {
            let! user = Database.getUser db userid

            match user with
            | Some user ->
                return!
                    (Response.withContentType "application/json; charset=utf-8"
                     >> Response.ofPlainText (
                         Encode.Auto.toString<User> (4, user, extra = (Extra.empty |> Extra.withDecimal))
                     ))
                        ctx
            | None -> return! ErrorHandler.NotFound ctx
        }

let getDailyWorkLog db (user: UserId) (date: DateTime) : HttpHandler =
    fun ctx ->
        task {
            let! projects = Database.listUserProjects db user date

            match projects with
            | Ok projects ->
                return!
                    Handlers.mapJson
                        (projects
                         |> List.map DailyWorkLog.encoder
                         |> Encode.list)
                        ctx

            | Error exn ->
                printfn $"%A{exn}"
                return! Response.ofEmpty ctx
        }

let postDailyWorkLog db (user: UserId) (date: DateTime) : HttpHandler =
    fun ctx ->
        task {
            use reader = new IO.StreamReader(ctx.Request.Body)
            let! body = reader.ReadToEndAsync()

            match Decode.fromString (Decode.list DailyWorkLog.decoder) body with
            | Ok worklogs ->
                let projectWorkLogs =
                    worklogs
                    |> List.map (fun worklog ->
                        worklog.workUnits
                        |> List.map WorkUnit.validate
                        |> List.flattenOption
                        |> Option.map (fun workUnits -> (worklog.projectId, workUnits)))
                    |> List.flattenOption

                match projectWorkLogs with
                | Some projectWorkLogs ->
                    let! result = Database.assignProjectWorkUnits db user date projectWorkLogs

                    match result with
                    | Ok _ -> return Response.ofPlainText "Success" ctx
                    | Error exn ->
                        printfn $"%A{exn}"
                        return ErrorHandler.BadRequest "Could update database" ctx
                | None -> return ErrorHandler.BadRequest "DailyWorkLog contains invalid values" ctx
            | Error error -> return ErrorHandler.BadRequest error ctx
        }

let relatedIssues (user: UserId) : HttpHandler =
    let issues =
        [ { key = "HR-5"
            title = "Interview Bob" }
          { key = "SDA-51"
            title = "Crashes randomly" }
          { key = "SDA-42"
            title = "Moar Features!" } ]

    (Response.withContentType "application/json; charset=utf-8"
     >> Response.ofPlainText (Encode.Auto.toString<Issue list> (4, issues)))
