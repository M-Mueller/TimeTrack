module Server

open System
open System.Globalization

open Fumble
open Microsoft.Data.Sqlite

open Falco
open Falco.Routing
open Falco.HostBuilder
open Falco.Security
open Microsoft.AspNetCore.Builder

open Thoth.Json.Net

open Domain
open Auth
open Utils


[<EntryPoint>]
let main args =

    let connection =
        Sqlite.existingConnection (new SqliteConnection("Data Source=:memory:"))
    // Sqlite.existingConnection (new SqliteConnection("Data Source=db.sqlite"))

    Database.createTables connection

    Database.initTestData connection

    let handle401 =
        Response.withStatusCode 401
        >> Response.withHeader "WWW-Authenticate" "Basic"
        >> Response.ofPlainText "Authorization required"

    let handleBadRequest error =
        Response.withStatusCode 400
        >> Response.ofPlainText error

    let handleNotFound =
        Response.withStatusCode 404
        >> Response.ofPlainText "Not found"

    let getUserHandler (userid: UserId) : HttpHandler =
        fun ctx ->
            task {
                let! user = Database.getUser connection userid

                match user with
                | Some user ->
                    return!
                        (Response.withContentType "application/json; charset=utf-8"
                         >> Response.ofPlainText (
                             Encode.Auto.toString<User> (4, user, extra = (Extra.empty |> Extra.withDecimal))
                         ))
                            ctx
                | None -> return! handleNotFound ctx
            }

    let parseIsoDate (route: RouteCollectionReader) =
        route.TryGetString "date"
        |> Result.fromOption "Route requires a <data>"
        |> Result.bind (fun input ->
            try
                Ok(DateTime.ParseExact(input, "yyyy-MM-dd", CultureInfo.InvariantCulture))
            with
            | _ -> Error "<date> must be in ISO format (yyyy-MM-dd)")

    let getDailyWorkLogHandler (user: UserId) (date: DateTime) : HttpHandler =
        fun ctx ->
            task {
                let! projects = Database.listUserProjects connection user date

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

    let postDailyWorkLogHandler (user: UserId) (date: DateTime) : HttpHandler =
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
                        let! result = Database.assignProjectWorkUnits connection user date projectWorkLogs

                        match result with
                        | Ok _ -> return Response.ofPlainText "Success" ctx
                        | Error exn ->
                            printfn $"%A{exn}"
                            return handleBadRequest "Could update database" ctx
                    | None -> return handleBadRequest "DailyWorkLog contains invalid values" ctx
                | Error error -> return handleBadRequest error ctx
            }

    let relatedIssuesHandler (user: UserId) : HttpHandler =
        let issues =
            [ { key = "HR-5"
                title = "Interview Bob" }
              { key = "SDA-51"
                title = "Crashes randomly" }
              { key = "SDA-42"
                title = "Moar Features!" } ]

        (Response.withContentType "application/json; charset=utf-8"
         >> Response.ofPlainText (Encode.Auto.toString<Issue list> (4, issues)))

    let requireAuthentication handleOk =
        ifAuthenticated (Database.authenticateUser connection) handleOk handle401

    webHost args {
        use_static_files
        use_if FalcoExtensions.IsDevelopment DeveloperExceptionPageExtensions.UseDeveloperExceptionPage

        endpoints [
            get "/api/v1/user" (requireAuthentication getUserHandler)
            get
                "/api/v1/dailyworklog/{date:required}"
                (requireAuthentication (fun user ->
                    Request.bindRoute parseIsoDate (getDailyWorkLogHandler user) handleBadRequest))
            post
                "/api/v1/dailyworklog/{date:required}"
                (requireAuthentication (fun user ->
                    Request.bindRoute parseIsoDate (postDailyWorkLogHandler user) handleBadRequest))

            get "/api/v1/relatedIssues" (requireAuthentication relatedIssuesHandler)
        ]
    }

    0
