namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module BasicTests =
    open Basic

    [<Fact>]
    let ``token matches the specified character`` () =
        let state = StringParseState ("abc", 0)
        let actual = token 'a' state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``token doesn't match a different character`` () =
        let state = StringParseState ("abc", 0)
        let actual = token 'b' state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, state

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``tokenNot matches a different character`` () =
        let state = StringParseState ("abc", 0)
        let actual = tokenNot 'x' state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``tokenNot doesn't match the specified character`` () =
        let state = StringParseState ("abc", 0)
        let actual = tokenNot 'a' state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, state

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``tokenPred matches a matching character`` () =
        let state = StringParseState ("abc", 0)
        let actual = tokenPred Char.IsLower state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``tokenPred doesn't match a non matching character`` () =
        let state = StringParseState ("ABC", 0)
        let actual = tokenPred Char.IsLower state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, state

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``value returns the specified value`` () =
        let state = StringParseState ("abc", 0)
        let actual = value 'd' state
        let expected = Success { Consumed = false; Value = 'd' }, state

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``any returns any token if there is data`` () =
        let state = StringParseState ("abc", 0)
        let actual = any state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``any fails if there is no more data`` () =
        let state = StringParseState ("abc", 3)
        let actual = any state
        let expected = Failure { Consumed = false; Message = "End of input." }, state

        Assert.Equal<Result<char> * ParseState>(expected, actual)