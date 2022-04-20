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

    let handle400 error =
        Response.withStatusCode 400
        >> Response.ofPlainText error

    let parseIsoDate (route: RouteCollectionReader) =
        route.TryGetString "date"
        |> Result.fromOption "Route requires a <data>"
        |> Result.bind (fun input ->
            try
                Ok(DateTime.ParseExact(input, "yyyy-MM-dd", CultureInfo.InvariantCulture))
            with
            | _ -> Error "<date> must be in ISO format (yyyy-MM-dd)")

    let dailyWorkLogHandler (user: User) (date: DateTime) : HttpHandler =
        fun ctx ->
            task {
                let! projects = Database.listUserProjects connection user.name date

                match projects with
                | Ok projects ->
                    return!
                        (Response.withContentType "application/json; charset=utf-8"
                         >> Response.ofPlainText (
                             Encode.Auto.toString<DailyWorkLog list> (
                                 4,
                                 projects,
                                 extra = (Extra.empty |> Extra.withDecimal)
                             )
                         ))
                            ctx
                | Error exn ->
                    printfn $"%A{exn}"
                    return! Response.ofEmpty ctx
            }

    let relatedIssuesHandler (user: User) : HttpHandler =
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
            get
                "/api/v1/dailyworklog/{date:required}"
                (requireAuthentication (fun user -> Request.bindRoute parseIsoDate (dailyWorkLogHandler user) handle400))

            get "/api/v1/relatedIssues" (requireAuthentication relatedIssuesHandler)
        ]
    }

    0
