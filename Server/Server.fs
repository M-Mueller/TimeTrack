module Server

open Suave

startWebServer defaultConfig (Successful.OK "Hello World!")