module Database

open Project
open Fumble
open Utils

let createTables connection =
    connection
    |> Sqlite.command
        """
        PRAGMA foreign_keys = ON;
        CREATE TABLE Users (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL, password BLOB NOT NULL, salt BLOB NOT NULL);
        CREATE TABLE Projects (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL);
        CREATE TABLE ScheduledProjects (
            project INTEGER NOT NULL REFERENCES Projects(id) ON DELETE CASCADE,
            user INTEGER NOT NULL REFERENCES Users(id) ON DELETE CASCADE,
            month INTEGER NOT NULL,
            year INTEGER NOT NULL,
            scheduledHours INTEGER NOT NULL,
            UNIQUE(project, user, month, year)
        );
        CREATE TABLE WorkUnits (
            project INTEGER NOT NULL REFERENCES Projects(id) ON DELETE CASCADE,
            user INTEGER NOT NULL REFERENCES Users(id) ON DELETE CASCADE,
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
        "INSERT INTO Users(id, name, password, salt) VALUES (null, @name, @pwd, @salt)",
        [ [ "@name", Sqlite.string "admin"
            "@pwd", Sqlite.bytes hash
            "@salt", Sqlite.bytes salt ] ]

        "INSERT INTO Projects(id, name) VALUES (null, @name)",
        [ [ "@name", Sqlite.string "HR" ]
          [ "@name", Sqlite.string "Admin" ]
          [ "@name", Sqlite.string "Super Duper App" ] ]

        "INSERT INTO ScheduledProjects(project, user, month, year, scheduledHours) VALUES (@project, @user, @month, @year, @scheduled)",
        [ [ "@project", Sqlite.int 1
            "@user", Sqlite.int 1
            "@month", Sqlite.int 4
            "@year", Sqlite.int 2022
            "@scheduled", Sqlite.int 12 ]
          [ "@project", Sqlite.int 3
            "@user", Sqlite.int 1
            "@month", Sqlite.int 4
            "@year", Sqlite.int 2022
            "@scheduled", Sqlite.int 42 ]
          [ "@project", Sqlite.int 1
            "@user", Sqlite.int 1
            "@month", Sqlite.int 5
            "@year", Sqlite.int 2022
            "@scheduled", Sqlite.int 24 ] ]

        "INSERT INTO WorkUnits(project, user, day, hours, comment) VALUES (@project, @user, @day, @hours, @comment)",
        [ [ "@project", Sqlite.int 1
            "@user", Sqlite.int 1
            "@day", Sqlite.string "2022-04-07"
            "@hours", Sqlite.int 4
            "@comment", Sqlite.string "Interviewing Bob" ]
          [ "@project", Sqlite.int 1
            "@user", Sqlite.int 1
            "@day", Sqlite.string "2022-04-06"
            "@hours", Sqlite.int 3
            "@comment", Sqlite.string "Interviewing John" ] ]
       ]
    |> (fun result ->
        match result with
        | Ok rows -> printfn $"Inserted {List.sum rows} rows"
        | Error exn -> failwith exn.Message)



type internal UserDTO =
    { id: int64
      name: string
      hash: byte []
      salt: byte [] }

let authenticateUser connection (name: string) (password: string) : Async<User option> =
    connection
    |> Sqlite.query "SELECT id, password, salt FROM Users WHERE name=@name"
    |> Sqlite.parameters [
        "@name", Sqlite.string name
       ]
    |> Sqlite.executeAsync (fun read ->
        { id = read.int64 "id"
          name = name
          hash = read.bytes "password"
          salt = read.bytes "salt" })
    |> Async.map (function
        | Ok users ->
            users
            |> List.tryHead
            |> Option.bind (fun user ->
                if (Crypto.sha256 password user.salt) = user.hash then
                    Some { id = user.id; name = user.name }
                else
                    None)
        | Error exn -> failwith exn.Message)

let listProjects connection : Async<Result<ProjectName list, exn>> =
    connection
    |> Sqlite.query "SELECT name FROM Projects"
    |> Sqlite.executeAsync (fun read -> read.string "name")

type internal WorkUnitDTO =
    { id: int64
      name: string
      scheduledHours: decimal
      day: System.DateTime option
      hours: decimal option
      comment: string option }

let listUserProjects connection (username: string) (date: System.DateTime) : Async<Result<ScheduledProject list, exn>> =
    // Combines rows repesenting WorkUnits of the same project into one ScheduledProject
    let combineRows (projects: Map<int64, ScheduledProject>) (row: WorkUnitDTO) =
        // Check whether we encountered this project before or create a new project otherwise
        let project =
            projects.TryFind row.id
            |> Option.defaultValue
                { name = row.name
                  scheduledHours = decimal row.scheduledHours
                  committedHoursOtherDays = decimal 0
                  workUnits = [] }

        // Either all values are set or not. Otherwise the query is wrong
        assert
            (Option.isSome row.day
             && Option.isSome row.hours
             && Option.isSome row.comment)
            || (Option.isNone row.day
                && Option.isNone row.hours
                && Option.isNone row.comment)

        let project =
            // If this row represents a valid WorkUnit (can be null if a project doesn't have any associated WorkUnits)
            match row.day, row.hours, row.comment with
            | Some day, Some hours, Some comment ->
                if day = date then
                    // Add WorkUnits of the requested date
                    { project with
                        workUnits =
                            { WorkUnit.hours = string hours
                              comment = comment }
                            :: project.workUnits }
                else
                    // Otherwise just add up the hours
                    { project with
                        committedHoursOtherDays =
                            project.committedHoursOtherDays
                            + (Option.defaultValue 0m row.hours) }
            | _ -> project

        projects.Add(row.id, project)


    connection
    |> Sqlite.query
        """
        SELECT p.id, p.name, sp.scheduledHours, wu.day, wu.hours, wu.comment
        FROM Projects AS p
        LEFT JOIN ScheduledProjects AS sp
        ON sp.project=p.id AND month=@month AND year=@year
        LEFT JOIN Users 
        ON sp.user=Users.id AND Users.name=@username
        LEFT JOIN WorkUnits AS wu
        ON wu.project=p.id AND wu.User=Users.id
        """
    |> Sqlite.parameters [
        "@username", Sqlite.string username
        "@month", Sqlite.int date.Month
        "@year", Sqlite.int date.Year
       ]
    |> Sqlite.executeAsync (fun read ->
        { id = read.int64 "id"
          name = read.string "name"
          scheduledHours =
            read.decimalOrNone "scheduledHours"
            |> Option.defaultValue 0m
          day = read.dateTimeOrNone "day"
          hours = read.decimalOrNone "hours"
          comment = read.stringOrNone "comment" })
    |> Async.map (
        Result.map (
            List.fold combineRows (Map [])
            >> Map.values
            >> Seq.toList
        )
    )
