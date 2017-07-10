namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module RecursiveTests =
    open Basic
    open Sequence

    let rec parenthesised state =
        digitInsideParens
        |> between (token '(') (token ')') <| state
    and digitInsideParens state =
         either (tokenPred Char.IsDigit) parenthesised <| state

    [<Fact>]
    let ``digitInsideParens succeeds if the number of parens match`` () =
        digitInsideParens |> Util.succeed "(((5)))" '5'

    [<Fact>]
    let ``digitInsideParens fails if the number of parens don't match`` () =
        digitInsideParens |> Util.fails "(((5))" true

    [<Fact>]
    let ``digitInsideParens fails if the there is no digit inside`` () =
        digitInsideParens |> Util.fails "(((a)))" true