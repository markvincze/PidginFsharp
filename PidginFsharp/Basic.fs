namespace PidginFsharp

module Basic =
    let tokenPred pred state =
        match ParserState.peek state with
        | Some t when (pred t) -> (Types.parseSuccess true t, ParserState.advance state)
        | Some t -> (Types.parseFailure false "Not matching token.", state)
        | _ -> (Types.parseFailure false "End of input.", state)

    let token t = tokenPred (fun t' -> t' = t)

    let tokenNot t = tokenPred (fun t' -> t' <> t)

    let value t state = Types.parseSuccess false t, state

    let any state =
        match ParserState.peek state with
        | Some t -> (Types.parseSuccess true t, ParserState.advance state)
        | _ -> (Types.parseFailure false "End of input.", state)

    let select func parser state =
        match parser state with
        | Failure r, state -> Failure r, state
        | Success r, state -> Types.parseSuccess r.Consumed (func r.Value), state