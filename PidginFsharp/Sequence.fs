namespace PidginFsharp

module Sequence =
    open Basic

    let after parser1 parser2 state =
        match parser1 state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match parser2 state with
                               | Failure r2, state -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message, state
                               | Success r2, state -> Types.parseSuccess (r1.Consumed || r2.Consumed) r2.Value, state

    let before suffix parser state =
        match parser state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match suffix state with
                               | Failure r2, state -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message, state
                               | Success r2, state -> Types.parseSuccess (r1.Consumed || r2.Consumed) r1.Value, state

    let bindAndTransform next transform parser state =
        match parser state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match (next r1.Value) state with
                               | Failure r2, state -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message, state
                               | Success r2, state -> Types.parseSuccess (r1.Consumed || r2.Consumed) (transform r1.Value r2.Value), state

    let bind next = bindAndTransform next (fun r1 r2 -> r2)

    let oneOf parsers state =
        let rec oneOfRec parsers state failures =
            match parsers with
            | [] -> (Types.parseFailure false (if List.isEmpty failures then "No parser was specified." else (List.head failures).Message), state)
            | p :: rest -> match p state with
                           | (Success r, state) -> (Success r, state)
                           | (Failure r, state) when r.Consumed -> (Failure r, state)
                           | (Failure r, state) -> (oneOfRec rest state (r :: failures))

        oneOfRec parsers state []

    let either p1 p2 = oneOf [p1; p2]

    let sequence parsers state =
        let rec sequenceRec parsers state results consumed =
            match parsers with
            | [] -> (Types.parseSuccess consumed (List.rev results), state)
            | p :: rest -> match p state with
                           | (Success r, state) -> sequenceRec rest state (r.Value :: results) (consumed || r.Consumed)
                           | (Failure r, state) -> Types.parseFailure (consumed || r.Consumed) r.Message, state

        sequenceRec parsers state [] false

    let sequenceToken tokens = sequence (List.map token tokens)

    let string str =
        sequenceToken (String.explode str)
        |> select String.implode

    let map1 func parser state =
        match parser state with
        | Failure r, state -> Failure r, state
        | Success r, state -> Types.parseSuccess r.Consumed (func r.Value), state

    let map2 func parser1 parser2 state =
        match parser1 state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match parser2 state with
                               | Failure r2, state -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message, state
                               | Success r2, state -> Types.parseSuccess (r1.Consumed || r2.Consumed) (func r1.Value r2.Value), state

    let map3 func parser1 parser2 parser3 state =
        match parser1 state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match parser2 state with
                               | Failure r2, state -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message, state
                               | Success r2, state -> match parser3 state with
                                                      | Failure r3, state -> Types.parseFailure (r1.Consumed || r2.Consumed || r3.Consumed) r3.Message, state
                                                      | Success r3, state -> Types.parseSuccess (r1.Consumed || r2.Consumed || r3.Consumed) (func r1.Value r2.Value r3.Value), state

    let many parser state =
        let rec manyRec parser state consumed results =
            match parser state with
            | Failure r, state when r.Consumed -> Failure r, state
            | Failure r, state -> Types.parseSuccess consumed (List.rev results), state
            | Success r, state -> manyRec parser state (consumed || r.Consumed) (r.Value :: results)

        manyRec parser state false []

    let atLeastOnce parser state =
        match parser state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match many parser state with
                               | Failure r2, state -> Failure r2, state
                               | Success r2, state -> Types.parseSuccess (r1.Consumed || r2.Consumed) (r1.Value :: r2.Value), state

    let between start ending main = map3 (fun r1 r2 r3 -> r2) start main ending

    let separatedAtLeastOnce separator parser state =
        match parser state with
        | Failure r1, state -> Failure r1, state
        | Success r1, state -> match many (after separator parser) state with
                               | Failure r2, state -> Failure r2, state
                               | Success r2, state -> Types.parseSuccess (r1.Consumed || r2.Consumed) (r1.Value :: r2.Value), state

    let separated separator parser = either (separatedAtLeastOnce separator parser) (value [])