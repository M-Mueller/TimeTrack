[<RequireQualifiedAccess>]
module UI.RelatedIssues

open Domain
open Feliz


let render (dispatchWriteToClipboard: string -> unit) (issues: Issue list) =
    let renderIssue issue =
        Html.div [
            prop.className "doodle-border"
            prop.style [
                style.margin 6
                style.position.relative
            ]
            prop.children [
                Html.a [
                    prop.href $"http://example.com/issues/{issue.key}"
                    prop.text issue.key
                    prop.style [
                        style.display.block
                        style.fontSize (length.pt 16)
                    ]
                ]
                Html.a [
                    prop.style [
                        style.cursor.pointer
                        style.position.absolute
                        style.right 2
                        style.top 2
                    ]
                    prop.text "📋"
                    prop.title "Copy to clipboard"
                    prop.onClick (fun _ -> dispatchWriteToClipboard $"[{issue.key}] {issue.title}")

                    ]
                Html.text issue.title
            ]
        ]

    Html.div [
        prop.className "doodle-border"
        prop.style [
            style.maxWidth (length.px 300)
            style.marginLeft 20
        ]
        prop.children (
            Html.text "Related JIRA Issues"
            :: List.map renderIssue issues
        )
    ]
