module Handlers

open Falco
open Thoth.Json.Net

let mapJson (value: JsonValue) : HttpHandler =
    Response.withContentType "application/json; charset=utf-8"
     >> Response.ofPlainText (Encode.toString 2 value)
