namespace PidginFsharp

module ParserState =
    let peek state =
        match state with
        | StringParseState (s, i) -> if i >= 0 && i < String.length s
                                     then Seq.item i s |> Some
                                     else None

    let advance state =
        match state with
        | StringParseState (s, i) -> StringParseState (s, i + 1)