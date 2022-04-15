module Server

open Microsoft.Data.Sqlite
open Suave
open Suave.Filters
open Suave.Operators
open Fumble
open Thoth.Json.Net
open Project


[<EntryPoint>]
let main args =
    let connection =
        Sqlite.existingConnection (new SqliteConnection("Data Source=:memory:"))
    // Sqlite.existingConnection (new SqliteConnection("Data Source=db.sqlite"))

    Database.createTables connection

    Database.initTestData connection

    printfn
        "%A"
        (Database.listProjects connection
         |> Async.RunSynchronously)

    printfn
        "%A"
        (Database.listUserProjects connection "admin" (System.DateTime(2022, 4, 7))
         |> Async.RunSynchronously)

    let getProjects: WebPart =
        fun (context: HttpContext) ->
            async {
                let username = context.userState[Authentication.UserNameKey] :?> string

                let! projects = Database.listUserProjects connection username (System.DateTime(2022, 4, 7))

                match projects with
                | Ok projects ->
                    return!
                        Successful.OK
                            (Encode.Auto.toString<ScheduledProject list> (
                                4,
                                projects,
                                extra = (Extra.empty |> Extra.withDecimal)
                            ))
                            context
                | Error exn ->
                    printfn $"%A{exn}"
                    return! ServerErrors.INTERNAL_ERROR "An internal database error occured" context
            }

    let app =
        choose [
            Authentication.authenticateBasicAsync
                (fun (username, password) ->
                    async {
                        let! result = Database.authenticateUser connection username password

                        return
                            match result with
                            | Ok user -> user <> None
                            | Error exn ->
                                printfn $"%A{exn}"
                                false
                    })
                (choose [
                    path "/api/v1/projects"
                    >=> choose [ GET >=> getProjects ]

                    GET >=> Files.browseHome

                    RequestErrors.NOT_FOUND "Page not found."
                 ])

            ]

    let homeFolder = System.IO.Path.GetFullPath "../Client/public"
    printfn $"Using home folder: {homeFolder}"

    startWebServer { defaultConfig with homeFolder = Some homeFolder } app

    0
