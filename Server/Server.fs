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

    let parseIsoDate (route: RouteCollectionReader) =
        route.TryGetString "date"
        |> Result.fromOption "Route requires a <data>"
        |> Result.bind (fun input ->
            try
                Ok(DateTime.ParseExact(input, "yyyy-MM-dd", CultureInfo.InvariantCulture))
            with
            | _ -> Error "<date> must be in ISO format (yyyy-MM-dd)")

    let requireAuthentication handleOk =
        ifAuthenticated (Database.authenticateUser connection) handleOk ErrorHandler.Unauthorized

    webHost args {
        use_static_files
        use_if FalcoExtensions.IsDevelopment DeveloperExceptionPageExtensions.UseDeveloperExceptionPage

        endpoints [
            get "/api/v1/user" (requireAuthentication (ApiHandler.getUser connection))
            get
                "/api/v1/dailyworklog/{date:required}"
                (requireAuthentication (fun user ->
                    Request.bindRoute parseIsoDate (ApiHandler.getDailyWorkLog connection user) ErrorHandler.BadRequest))
            post
                "/api/v1/dailyworklog/{date:required}"
                (requireAuthentication (fun user ->
                    Request.bindRoute parseIsoDate (ApiHandler.postDailyWorkLog connection user) ErrorHandler.BadRequest))

            get "/api/v1/relatedIssues" (requireAuthentication ApiHandler.relatedIssues)
        ]
    }

    0
