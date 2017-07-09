namespace PidginFsharp.Test

open PidginFsharp
open Xunit

module Util =
    open Basic

    let succeed str result parser =
        let state = StringParseState(str)
        let actual = parser state
        let expected = Success { Consumed = true; Value = result }

        Assert.Equal<'T>(expected, actual)

    let fails str consumed parser =
        let state = StringParseState(str)
        let actual = parser state
        
        match actual with
        | Failure f -> Assert.Equal<bool>(consumed, f.Consumed)
        | Success _ -> Assert.True(false, "The parser should fail.")