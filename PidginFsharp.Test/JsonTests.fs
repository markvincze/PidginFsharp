namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module JsonTests =
    open JsonParser

    [<Fact>]
    let ``string matches a string`` () =
        let state = StringParseState ("\"abc\"", 0)
        let actual = string state
        let expected = Success { Consumed = true; Value = "abc" }, StringParseState ("\"abc\"", 5)

        Assert.Equal<Result<string> * ParseState>(expected, actual)

    [<Fact>]
    let ``json matches a Json string`` () =
        let state = StringParseState ("\"abc\"", 0)
        let actual = json state
        let expected = Success { Consumed = true; Value = JsonString "abc" }, StringParseState ("\"abc\"", 5)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)

    [<Fact>]
    let ``json matches a Json array`` () =
        let state = StringParseState ("[\"abc\"]", 0)
        let actual = json state
        let expected = Success { Consumed = true; Value = JsonArray [ JsonString "abc" ] }, StringParseState ("[\"abc\"]", 7)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)

    [<Fact>]
    let ``json matches a nested Json array`` () =
        let state = StringParseState ("[[[\"abc\"]]]", 0)
        let actual = json state
        let expected = Success { Consumed = true; Value = JsonArray [ JsonArray [ JsonArray [ JsonString "abc" ] ] ] }, StringParseState ("[[[\"abc\"]]]", 11)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)