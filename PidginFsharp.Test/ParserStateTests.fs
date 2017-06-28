namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module ParserStateTests =
    [<Fact>]
    let ``peek returns the first character`` () =
        let actual = StringParseState ("abc", 0) |> ParserState.peek
        let expected = Some 'a'
        Assert.Equal<char option>(expected, actual)

    [<Fact>]
    let ``advance moves one character further`` () =
        let actual = StringParseState ("abc", 0) |> ParserState.advance |> ParserState.peek
        let expected = Some 'b'
        Assert.Equal<char option>(expected, actual)

    [<Fact>]
    let ``after advancing after the end peek returns None`` () =
        let state = StringParseState ("abc", 0)
                     |> ParserState.advance
                     |> ParserState.advance
                     |> ParserState.advance

        let actual = ParserState.peek state
        let expected = None
        Assert.Equal<char option>(expected, actual)