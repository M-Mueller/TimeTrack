namespace Domain

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type ProjectName = string

/// Represents a single JIRA issue
type Issue = { key: string; title: string }

module Issue =
    let decoder: Decoder<Issue> =
        Decode.object (fun get ->
            { key = get.Required.Field "key" Decode.string
              title = get.Required.Field "title" Decode.string })
