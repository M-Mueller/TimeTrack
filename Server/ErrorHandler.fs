module ErrorHandler

open Falco

let Unauthorized : HttpHandler =
    Response.withStatusCode 401
    >> Response.withHeader "WWW-Authenticate" "Basic"
    >> Response.ofPlainText "Authorization required"

let BadRequest message : HttpHandler =
    Response.withStatusCode 400
    >> Response.ofPlainText message

let NotFound : HttpHandler =
    Response.withStatusCode 404
    >> Response.ofPlainText "Not found"
