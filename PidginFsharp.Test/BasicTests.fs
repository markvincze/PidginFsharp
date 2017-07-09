namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module BasicTests =
    open Basic

    [<Fact>]
    let ``token matches the specified character`` () =
        token 'a' |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``token doesn't match a different character`` () =
        token 'b' |> Util.fails "abc" false

    [<Fact>]
    let ``tokenNot matches a different character`` () =
        tokenNot 'x' |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``tokenNot doesn't match the specified character`` () =
        tokenNot 'a' |> Util.fails "abc" false

    [<Fact>]
    let ``tokenPred matches a matching character`` () =
        tokenPred Char.IsLower |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``tokenPred doesn't match a non matching character`` () =
        tokenPred Char.IsLower |> Util.fails "ABC" false

    [<Fact>]
    let ``value returns the specified value without consuming input`` () =
        let state = StringParseState("abc")
        let actual = value 'd' state
        let expected = Success { Consumed = false; Value = 'd' }

        Assert.Equal<Result<char>>(expected, actual)

    [<Fact>]
    let ``any returns any token if there is data`` () =
        any |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``any fails if there is no more data`` () =
        let state = StringParseState("abc")
        state |> ParserState.advance
        state |> ParserState.advance
        state |> ParserState.advance

        let actual = any state
        let expected = Failure { Consumed = false; Message = "End of input." }

        Assert.Equal<Result<char>>(expected, actual)