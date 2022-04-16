module Server

open System

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


[<EntryPoint>]
let main args =

    let connection =
        Sqlite.existingConnection (new SqliteConnection("Data Source=:memory:"))
    // Sqlite.existingConnection (new SqliteConnection("Data Source=db.sqlite"))

    Database.createTables connection

    Database.initTestData connection

    let projectsHandler (user: User) : HttpHandler =
        fun ctx ->
            task {
                let! projects = Database.listUserProjects connection user.name (System.DateTime(2022, 4, 7))

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

    let handle401 =
        Response.withStatusCode 401
        >> Response.withHeader "WWW-Authenticate" "Basic"
        >> Response.ofPlainText "Authorization required"

    let requireAuthentication handleOk =
        ifAuthenticated (Database.authenticateUser connection) handleOk handle401

    webHost args {
        use_static_files
        use_if FalcoExtensions.IsDevelopment DeveloperExceptionPageExtensions.UseDeveloperExceptionPage

        endpoints [
            get "/api/v1/projects" (requireAuthentication projectsHandler)
        ]
    }

    0
