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
        let state = StringParseState ("[\"abc\", \"def\" ]", 0)
        let actual = json state
        let expected = Success { Consumed = true; Value = JsonArray [ JsonString "abc"; JsonString "def" ] }, StringParseState ("[\"abc\", \"def\" ]", 15)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)

    [<Fact>]
    let ``json matches a nested Json array`` () =
        let state = StringParseState ("[[[\"abc\"]]]", 0)
        let actual = json state
        let expected = Success { Consumed = true; Value = JsonArray [ JsonArray [ JsonArray [ JsonString "abc" ] ] ] }, StringParseState ("[[[\"abc\"]]]", 11)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)

    [<Fact>]
    let ``json matches a simple object`` () =
        let state = StringParseState ("{ \"foo\": \"bar\" }", 0)
        let actual = json state
        let expected = Success { Consumed = true; Value = JsonObject (["foo", JsonString "bar"] |> Map.ofList) }, StringParseState ("{ \"foo\": \"bar\" }", 16)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)
    
    [<Fact>]
    let ``json correctly parses complex Json`` () =
        let jsonStr = """
            [
                {
                    "prop1"  : "val1",
                    "prop2" : { "prop3" : "val2" }
                },
                [
                    "val3",
                    { "prop4": "val4" }
                ]
            ]"""
        let state = StringParseState (jsonStr, 0)

        let actual = json state
        
        let expectedJson =
            JsonArray [
                JsonObject
                    (Map [
                        "prop1", JsonString "val1"
                        "prop2", JsonObject (Map ["prop3", JsonString "val2"])
                    ])
                JsonArray [
                    JsonString "val3"
                    JsonObject (Map ["prop4", JsonString "val4"])
                ]
            ]

        let expected = Success { Consumed = true; Value = expectedJson }, StringParseState (jsonStr, 269)

        Assert.Equal<Result<Json> * ParseState>(expected, actual)