namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module StringTests =
    [<Fact>]
    let ``explode returns empty list for empty string`` () =
        let actual = String.explode ""
        let expected = []
        Assert.Equal<char list>(expected, actual)

    [<Fact>]
    let ``explode returns empty list for null`` () =
        let actual = String.explode null
        let expected = []
        Assert.Equal<char list>(expected, actual)

    [<Fact>]
    let ``explode returns the correct list of characters`` () =
        let actual = String.explode "abc"
        let expected = ['a'; 'b'; 'c']
        Assert.Equal<char list>(expected, actual)

    [<Fact>]
    let ``implode returns empty string for empty list`` () =
        let actual = String.implode []
        let expected = ""
        Assert.Equal<string>(expected, actual)

    [<Fact>]
    let ``implode returns the correct string`` () =
        let actual = String.implode ['a'; 'b'; 'c']
        let expected = "abc"
        Assert.Equal<string>(expected, actual)