module Database

open Project
open Fumble
open Suave.Utils

let createTables connection =
    connection
    |> Sqlite.command
        """
        PRAGMA foreign_keys = ON;
        CREATE TABLE Users (id INTEGER PRIMARY KEY, email TEXT UNIQUE NOT NULL, password BLOB NOT NULL, salt BLOB NOT NULL);
        CREATE TABLE Projects (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL);
        CREATE TABLE ScheduledProjects (
            project INTEGER REFERENCES Projects(id) ON DELETE CASCADE,
            month INTEGER NOT NULL,
            year INTEGER NOT NULL,
            scheduledHours INTEGER NOT NULL
        );
        CREATE TABLE WorkUnits (
            project INTEGER REFERENCES Projects(id) ON DELETE CASCADE,
            day TEXT NOT NULL,
            hours TEXT NOT NULL,
            comment TEXT NOT NULL
        );
        """
    |> Sqlite.executeCommand
    |> (fun result ->
        match result with
        | Ok rows -> printfn "Created tables"
        | Error exn -> failwith exn.Message)

let initTestData connection =
    let salt = Crypto.salt ()
    let hash = Crypto.sha256 "admin" salt

    connection
    |> Sqlite.executeTransaction [
        "INSERT INTO Users(id, email, password, salt) VALUES (null, @email, @pwd, @salt)",
        [ [ "@email", Sqlite.string "admin@example.com"
            "@pwd", Sqlite.bytes hash
            "@salt", Sqlite.bytes salt ] ]

        "INSERT INTO Projects(id, name) VALUES (null, @name)",
        [ [ "@name", Sqlite.string "HR" ]
          [ "@name", Sqlite.string "Admin" ]
          [ "@name", Sqlite.string "Super Duper App" ] ]

        "INSERT INTO ScheduledProjects(project, month, year, scheduledHours) VALUES (@project, @month, @year, @scheduled)",
        [ [ "@project", Sqlite.int 1
            "@month", Sqlite.int 4
            "@year", Sqlite.int 2022
            "@scheduled", Sqlite.int 12 ]
          [ "@project", Sqlite.int 3
            "@month", Sqlite.int 4
            "@year", Sqlite.int 2022
            "@scheduled", Sqlite.int 42 ] ]

        "INSERT INTO WorkUnits(project, day, hours, comment) VALUES (@project, @day, @hours, @comment)",
        [ [ "@project", Sqlite.int 1
            "@day", Sqlite.string "2022-04-07"
            "@hours", Sqlite.int 4
            "@comment", Sqlite.string "Interviewing Bob" ] ]
       ]
    |> (fun result ->
        match result with
        | Ok rows -> printfn $"Inserted {List.sum rows} rows"
        | Error exn -> failwith exn.Message)



type internal UserDTO =
    { id: int64
      email: string
      hash: byte []
      salt: byte [] }

let authenticateUser connection (email: string) (password: string) =
    connection
    |> Sqlite.query "SELECT id, password, salt FROM Users WHERE email=@email"
    |> Sqlite.parameters [
        "@email", Sqlite.string email
       ]
    |> Sqlite.executeAsync
        (fun read ->
            { id = read.int64 "id"
              email = email
              hash = read.bytes "password"
              salt = read.bytes "salt" })
    |> Async.map (
        Result.map
            (fun users ->
                match List.tryHead users with
                | Some user ->
                    if (Crypto.sha256 password user.salt) = user.hash then
                        Some { id = user.id; email = user.email }
                    else
                        None
                | None -> None)
    )

let listProjects connection : Async<Result<ProjectName list, exn>> =
    connection
    |> Sqlite.query "SELECT name from Projects"
    |> Sqlite.executeAsync (fun read -> read.string "name")
