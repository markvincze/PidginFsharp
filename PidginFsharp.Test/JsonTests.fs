namespace PidginFsharp.Test

open PidginFsharp
open PidginFsharp.Examples
open System
open Xunit
open JsonParser

module JsonTests =
    open JsonParser

    [<Fact>]
    let ``string matches a string`` () =
        string |> Util.succeed "\"abc\"" "abc"

    [<Fact>]
    let ``json matches a Json string`` () =
        json |> Util.succeed "\"abc\"" (JsonString "abc")

    [<Fact>]
    let ``json matches a Json array`` () =
        json
        |> Util.succeed
            "[\"abc\", \"def\" ]"
            (JsonArray [ JsonString "abc"; JsonString "def" ])

    [<Fact>]
    let ``json matches a nested Json array`` () =
        json
        |> Util.succeed
            "[[[\"abc\"]]]"
            (JsonArray [ JsonArray [ JsonArray [ JsonString "abc" ] ] ])

    [<Fact>]
    let ``json matches a simple object`` () =
        json
        |> Util.succeed
            "{ \"foo\": \"bar\" }"
            (JsonObject (["foo", JsonString "bar"] |> Map.ofList))
    
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

        json |> Util.succeed jsonStr expectedJson