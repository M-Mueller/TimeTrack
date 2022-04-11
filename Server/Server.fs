module Server

open Microsoft.Data.Sqlite
open Suave
open Suave.Filters
open Suave.Operators
open Fumble
open Thoth.Json.Net


[<EntryPoint>]
let main args =
    let connection =
        Sqlite.existingConnection (new SqliteConnection("Data Source=:memory:"))
//        Sqlite.existingConnection (new SqliteConnection("Data Source=C:\Users\Mueller\Projects\TimeTrack\db.sqlite"))

    Database.createTables connection

    Database.initTestData connection

    let getProjects: WebPart =
        fun (context: HttpContext) ->
            async {
                let! projects = Database.listProjects connection

                match projects with
                | Ok projects -> return! Successful.OK(Encode.Auto.toString<string list> (4, projects)) context
                | Error exn ->
                    printfn $"%A{exn}"
                    return! ServerErrors.INTERNAL_ERROR "An internal database error occured" context
            }

    let app =
        choose [
            Authentication.authenticateBasicAsync
                (fun (email, password) ->
                    async {
                        let! result = Database.authenticateUser connection email password

                        return
                            match result with
                            | Ok user -> user <> None
                            | Error exn ->
                                printfn $"%A{exn}"
                                false
                    })
                (path "/projects"
                 >=> choose [ GET >=> getProjects ])
        ]

    startWebServer defaultConfig app

    0
