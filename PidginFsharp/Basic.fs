namespace PidginFsharp

module Basic =
    let tokenPred pred state =
        match ParserState.peek state with
        | Some t when (pred t) ->
            ParserState.advance state |> ignore
            Types.parseSuccess true t
        | Some t -> Types.parseFailure false "Not matching token."
        | _ -> Types.parseFailure false "End of input."

    let token t = tokenPred (fun t' -> t' = t)

    let tokenNot t = tokenPred (fun t' -> t' <> t)

    let value t state = Types.parseSuccess false t

    let any state =
        match ParserState.peek state with
        | Some t ->
            ParserState.advance state |> ignore
            Types.parseSuccess true t
        | _ -> Types.parseFailure false "End of input."

    let select func parser state =
        match parser state with
        | Failure r -> Failure r
        | Success r -> Types.parseSuccess r.Consumed (func r.Value)