namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module RecursiveTests =
    open Basic
    open Sequence

    let rec parenthesised state =
        state |>
        (before
            (after (token '(') digitInsideParens)
            (token ')'))
    and digitInsideParens state = state |> (either (tokenPred Char.IsDigit) parenthesised)

    [<Fact>]
    let ``digitInsideParens succeeds if the number of parens match`` () =
        let state = StringParseState ("(((5)))", 0)
        let actual = digitInsideParens state
        let expected = Success { Consumed = true; Value = '5' }, StringParseState ("(((5)))", 7)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``digitInsideParens fails if the number of parens don't match`` () =
        let state = StringParseState ("(((5))", 0)
        let actual = digitInsideParens state
        let expected = Failure { Consumed = true; Message = "End of input." }, StringParseState ("(((5))", 6)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``digitInsideParens fails if the there is no digit inside`` () =
        let state = StringParseState ("(((a)))", 0)
        let actual = digitInsideParens state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("(((a)))", 3)

        Assert.Equal<Result<char> * ParseState>(expected, actual)