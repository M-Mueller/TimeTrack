module Elements

open Feliz

/// An input with attached label and an optional validation error
let labeledInput (label: string) (error: string) (props: IReactProperty list) (inputProps: IReactProperty list) =
    Html.div [
        yield! props
        prop.children [
            Html.label [
                prop.className "labeledInput"
                prop.children [
                    Html.text label
                    Html.input (prop.className "labeledInput" :: inputProps)
                    Html.span [
                        prop.className "labeledInput"
                        prop.text error
                    ]
                ]
            ]
        ]
    ]
