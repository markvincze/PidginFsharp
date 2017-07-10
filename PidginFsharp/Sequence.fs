namespace PidginFsharp

module Sequence =
    open Basic
    open ParserState

    let after parser1 parser2 state =
        match parser1 state with
        | Failure r1 -> Failure r1
        | Success r1 -> match parser2 state with
                        | Failure r2 -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message
                        | Success r2 -> Types.parseSuccess (r1.Consumed || r2.Consumed) r2.Value

    let before suffix parser state =
        match parser state with
        | Failure r1 -> Failure r1
        | Success r1 -> match suffix state with
                        | Failure r2 -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message
                        | Success r2 -> Types.parseSuccess (r1.Consumed || r2.Consumed) r1.Value

    let bindAndTransform next transform parser state =
        match parser state with
        | Failure r1 -> Failure r1
        | Success r1 -> match (next r1.Value) state with
                        | Failure r2 -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message
                        | Success r2 -> Types.parseSuccess (r1.Consumed || r2.Consumed) (transform r1.Value r2.Value)

    let bind next = bindAndTransform next (fun r1 r2 -> r2)

    let oneOf parsers state =
        let rec oneOfRec parsers state failures =
            match parsers with
            | [] -> (Types.parseFailure false (if List.isEmpty failures then "No parser was specified." else (List.head failures).Message))
            | p :: rest -> match p state with
                           | (Success r) -> (Success r)
                           | (Failure r) when r.Consumed -> (Failure r)
                           | (Failure r) -> (oneOfRec rest state (r :: failures))

        oneOfRec parsers state []

    let either p1 p2 = oneOf [p1; p2]

    let sequence parsers state =
        let rec sequenceRec parsers state results consumed =
            match parsers with
            | [] -> (Types.parseSuccess consumed (List.rev results))
            | p :: rest -> match p state with
                           | (Success r) -> sequenceRec rest state (r.Value :: results) (consumed || r.Consumed)
                           | (Failure r) -> Types.parseFailure (consumed || r.Consumed) r.Message

        sequenceRec parsers state [] false

    let sequenceToken tokens = sequence (List.map token tokens)

    let string str =
        sequenceToken (String.explode str)
        |> select String.implode

    let map1 func parser state =
        match parser state with
        | Failure r -> Failure r
        | Success r -> Types.parseSuccess r.Consumed (func r.Value)

    let map2 func parser1 parser2 state =
        match parser1 state with
        | Failure r1 -> Failure r1
        | Success r1 -> match parser2 state with
                        | Failure r2 -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message
                        | Success r2 -> Types.parseSuccess (r1.Consumed || r2.Consumed) (func r1.Value r2.Value)

    let map3 func parser1 parser2 parser3 state =
        match parser1 state with
        | Failure r1 -> Failure r1
        | Success r1 -> match parser2 state with
                        | Failure r2 -> Types.parseFailure (r1.Consumed || r2.Consumed) r2.Message
                        | Success r2 -> match parser3 state with
                                        | Failure r3 -> Types.parseFailure (r1.Consumed || r2.Consumed || r3.Consumed) r3.Message
                                        | Success r3 -> Types.parseSuccess (r1.Consumed || r2.Consumed || r3.Consumed) (func r1.Value r2.Value r3.Value)

    let many parser state =
        let rec manyRec parser state consumed results =
            match parser state with
            | Failure r when r.Consumed -> Failure r
            | Failure r -> Types.parseSuccess consumed (List.rev results)
            | Success r -> manyRec parser state (consumed || r.Consumed) (r.Value :: results)

        manyRec parser state false []

    let atLeastOnce parser state =
        match parser state with
        | Failure r1 -> Failure r1
        | Success r1 -> match many parser state with
                        | Failure r2 -> Failure r2
                        | Success r2 -> Types.parseSuccess (r1.Consumed || r2.Consumed) (r1.Value :: r2.Value)

    let between start ending main = map3 (fun r1 r2 r3 -> r2) start main ending

    let separatedAtLeastOnce separator parser state =
        match parser state with
        | Failure r1 -> Failure r1
        | Success r1 -> match many (after separator parser) state with
                        | Failure r2 -> Failure r2
                        | Success r2 -> Types.parseSuccess (r1.Consumed || r2.Consumed) (r1.Value :: r2.Value)

    let separated separator parser = either (separatedAtLeastOnce separator parser) (value [])

    let probe parser state =
        pushBookmark state |> ignore
        match parser state with
        | Failure r -> 
            rewind state |> ignore
            Types.parseFailure false r.Message
        | Success s ->
            popBookmark state |> ignore
            Success s 