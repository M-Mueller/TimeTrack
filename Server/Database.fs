module Database

open Project
open Fumble

let createTables connection =
    connection
    |> Sqlite.command
        """
        PRAGMA foreign_keys = ON;
        CREATE TABLE Users (id INTEGER PRIMARY KEY, email TEXT UNIQUE NOT NULL, passwordhash TEXT NOT NULL);
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
    connection
    |> Sqlite.executeTransaction [
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


let listProjects connection : Async<Result<ProjectName list, exn>> =
    connection
    |> Sqlite.query "SELECT name from Projects"
    |> Sqlite.executeAsync (fun read -> read.string "name")
