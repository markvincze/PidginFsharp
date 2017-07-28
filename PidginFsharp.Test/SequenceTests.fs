namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module SequenceTests =
    open Basic
    open Sequence

    [<Fact>]
    let ``after fails if the first parser fails`` () =
        after (token 'x') (token 'b')
        |> Util.fails "abc" false

    [<Fact>]
    let ``after fails if the second parser fails`` () =
        after (token 'a') (token 'x')
        |> Util.fails "abc" true

    [<Fact>]
    let ``after returns the second result if both parsers succeed`` () =
        after (token 'a') (token 'b')
        |> Util.succeed "abc" 'b'

    [<Fact>]
    let ``before fails if the first parser fails`` () =
        before (token 'b') (token 'x')
        |> Util.fails "abc" false

    [<Fact>]
    let ``before fails if the second parser fails`` () =
        before (token 'x') (token 'a')
        |> Util.fails "abc" true

    [<Fact>]
    let ``before returns the first result if both parsers succeed`` () =
        before (token 'b') (token 'a')
        |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``bind fails if the first parser fails`` () =
        bind token (token 'x')
        |> Util.fails "abc" false

    [<Fact>]
    let ``bind returns the result of the second generated parser`` () =
        bind (fun c -> token (char (int c + 1))) (token 'a')
        |> Util.succeed "abc" 'b'

    [<Fact>]
    let ``bindAndTransform fails if the first parser fails`` () =
        bindAndTransform token (fun r1 r2 -> r1) (token 'x')
        |> Util.fails "abc" false

    [<Fact>]
    let ``bindAndTransform fails if the second parser fails`` () =
        bindAndTransform token (fun r1 r2 -> r1) (token 'a')
        |> Util.fails "abc" true

    [<Fact>]
    let ``bindAndTransform returns the result given by the transform function`` () =
        bindAndTransform
                        (fun c -> token (char (int c + 1)))
                        (fun r1 r2 -> r2.ToString() + r1.ToString())
                        (token 'a')
        |> Util.succeed "abc" "ba"

    [<Fact>]
    let ``oneOf fails if no parsers were specified`` () =
        oneOf []
        |> Util.fails "abc" false

    [<Fact>]
    let ``oneOf fails if all parsers fail`` () =
        oneOf [token 'x'; token 'y'; token 'z']
        |> Util.fails "abc" false

    [<Fact>]
    let ``oneOf returns the result of the succeeded parser`` () =
        oneOf [token 'x'; token 'y'; token 'a']
        |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``either fails if both parsers fail`` () =
        either (token 'x') (token 'y')
        |> Util.fails "abc" false

    [<Fact>]
    let ``either returns the result of the succeeded parser`` () =
        either (token 'x') (token 'a')
        |> Util.succeed "abc" 'a'

    [<Fact>]
    let ``sequence fails if any of the parsers fail`` () =
        sequence [token 'a'; token 'x'; token 'c']
        |> Util.fails "abc" true

    [<Fact>]
    let ``sequence succeeds if all parsers succeed`` () =
        sequence [token 'a'; token 'b'; token 'c']
        |> Util.succeed "abc" ['a'; 'b'; 'c']

    [<Fact>]
    let ``sequenceToken fails if any of the parsers fail`` () =
        sequenceToken ['a'; 'x'; 'c']
        |> Util.fails "abc" true

    [<Fact>]
    let ``sequenceToken succeeds if all parsers succeed`` () =
        sequenceToken ['a'; 'b'; 'c']
        |> Util.succeed "abc" ['a'; 'b'; 'c']

    [<Fact>]
    let ``string fails if string is different`` () =
        string "axc"
        |> Util.fails "abc" true

    [<Fact>]
    let ``string succeeds if string matches`` () =
        string "abc"
        |> Util.succeed "abc" "abc"

    [<Fact>]
    let ``map1 fails if parser fails`` () =
        map1 Char.ToUpper (token 'x')
        |> Util.fails "abc" false

    [<Fact>]
    let ``map1 returns the transformed output if the parser succeeds`` () =
        map1 Char.ToUpper (token 'a')
        |> Util.succeed "abc" 'A'

    [<Fact>]
    let ``map2 fails if the first parser fails`` () =
        map2 (fun c1 c2 -> c2) (token 'x') (token 'b')
        |> Util.fails "abc" false

    [<Fact>]
    let ``map2 fails if the second parser fails`` () =
        map2 (fun c1 c2 -> c2) (token 'a') (token 'x')
        |> Util.fails "abc" true

    [<Fact>]
    let ``map2 returns the transformed output if both parsers succeeds`` () =
        map2 (fun c1 c2 -> c1.ToString() + c2.ToString()) (token 'a') (token 'b')
        |> Util.succeed "abc" "ab"

    [<Fact>]
    let ``many returns success with empty result if no token matched`` () =
        let state = ParserState.createStringParserState "abc"
        let actual = state |> many (token 'x')
        let expected = Success { Consumed = false; Value = [] }

        Assert.Equal<Result<char list>>(expected, actual)

    [<Fact>]
    let ``many returns tokens until they match`` () =
        many (tokenPred Char.IsLower)
        |> Util.succeed "abcA" ['a'; 'b'; 'c']

    [<Fact>]
    let ``atLeastOnce fails if no token matches`` () =
        atLeastOnce (token 'x')
        |> Util.fails "abc" false

    [<Fact>]
    let ``atLeastOnce returns tokens until they match`` () =
        atLeastOnce (tokenPred Char.IsLower)
        |> Util.succeed "abc" ['a'; 'b'; 'c']

    [<Fact>]
    let ``atLeastOnce only one match`` () =
        atLeastOnce (tokenPred Char.IsLower)
        |> Util.succeed "a" ['a']

    [<Fact>]
    let ``between fails if the the beginning fails`` () =
        between (tokenPred Char.IsUpper) any any
        |> Util.fails "abc" false

    [<Fact>]
    let ``between fails if the the end fails`` () =
        between any (tokenPred Char.IsUpper) any
        |> Util.fails "abc" true

    [<Fact>]
    let ``between fails if the main parser fails`` () =
        between any any (tokenPred Char.IsUpper)
        |> Util.fails "abc" true

    [<Fact>]
    let ``between succeeds if beginning, end and the main parser succeed`` () =
        between (tokenPred Char.IsUpper) (tokenPred Char.IsUpper) any
        |> Util.succeed "AbC" 'b'

    [<Fact>]
    let ``separated succeeds if end of input`` () =
        let state = ParserState.createStringParserState ""
        let actual = state |> separated (token ' ') (string "foo")
        let expected = Success { Consumed = false; Value = [] }

        Assert.Equal<Result<string list>>(expected, actual)

    [<Fact>]
    let ``separated succeeds if matches once`` () =
        separated (token ' ') (string "foo")
        |> Util.succeed "foo" [ "foo" ]

    [<Fact>]
    let ``separated succeeds if matches twice`` () =
        separated (token ' ') (string "foo")
        |> Util.succeed "foo foo" [ "foo"; "foo" ]

    [<Fact>]
    let ``separated succeeds if matches once then no separator`` () =
        separated (token ' ') (string "foo")
        |> Util.succeed "foobar" [ "foo" ]

    [<Fact>]
    let ``separated succeeds if first doesn't match`` () =
        let state = ParserState.createStringParserState "bar"
        let actual = state |> separated (token ' ') (string "foo")
        let expected = Success { Consumed = false; Value = [ ] }

        Assert.Equal<Result<string list>>(expected, actual)

    [<Fact>]
    let ``separated fails if first partially match`` () =
        separated (token ' ') (string "foo")
        |> Util.fails "four" true

    [<Fact>]
    let ``separated fails if separator matches but not the next item`` () =
        separated (token ' ') (string "foo")
        |> Util.fails "foo bar" true

    let ``matching a second string fails without backtracking`` () =
        either (string "four") (string "foo")
        |> Util.fails "foo bar" true

    let ``matching a second string succeeds with backtracking`` () =
        either (probe (string "four")) (string "foo")
        |> Util.succeed "foo bar" "foo"