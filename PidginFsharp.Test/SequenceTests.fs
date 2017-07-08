namespace PidginFsharp.Test

open PidginFsharp
open System
open Xunit

module SequenceTests =
    open Basic
    open Sequence

    [<Fact>]
    let ``after fails if the first parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = after (token 'x') (token 'b') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``after fails if the second parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = after (token 'a') (token 'x') state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``after returns the second result if both parsers succeed`` () =
        let state = StringParseState ("abc", 0)
        let actual = after (token 'a') (token 'b') state
        let expected = Success { Consumed = true; Value = 'b' }, StringParseState ("abc", 2)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``before fails if the first parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = before (token 'b') (token 'x') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``before fails if the second parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = before (token 'x') (token 'a') state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``before returns the first result if both parsers succeed`` () =
        let state = StringParseState ("abc", 0)
        let actual = before (token 'b') (token 'a') state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 2)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``bind fails if the first parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = bind token (token 'x') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``bind returns the result of the second generated parser`` () =
        let state = StringParseState ("abc", 0)
        let actual = bind (fun c -> token (char (int c + 1))) (token 'a') state
        let expected = Success { Consumed = true; Value = 'b' }, StringParseState ("abc", 2)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``bindAndTransform fails if the first parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = bindAndTransform token (fun r1 r2 -> r1) (token 'x') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``bindAndTransform fails if the second parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = bindAndTransform token (fun r1 r2 -> r1) (token 'a') state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``bindAndTransform returns the result given by the transform function`` () =
        let state = StringParseState ("abc", 0)
        let actual = bindAndTransform
                        (fun c -> token (char (int c + 1)))
                        (fun r1 r2 -> r2.ToString() + r1.ToString())
                        (token 'a')
                        state

        let expected = Success { Consumed = true; Value = "ba" }, StringParseState ("abc", 2)

        Assert.Equal<Result<string> * ParseState>(expected, actual)

    [<Fact>]
    let ``oneOf fails if no parsers were specified`` () =
        let state = StringParseState ("abc", 0)
        let actual = oneOf [] state
        let expected = Failure { Consumed = false; Message = "No parser was specified." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``oneOf fails if all parsers fail`` () =
        let state = StringParseState ("abc", 0)
        let actual = oneOf [token 'x'; token 'y'; token 'z'] state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``oneOf returns the result of the succeeded parser`` () =
        let state = StringParseState ("abc", 0)
        let actual = oneOf [token 'x'; token 'y'; token 'a'] state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``either fails if both parsers fail`` () =
        let state = StringParseState ("abc", 0)
        let actual = either (token 'x') (token 'y') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``either returns the result of the succeeded parser`` () =
        let state = StringParseState ("abc", 0)
        let actual = either (token 'x') (token 'a') state
        let expected = Success { Consumed = true; Value = 'a' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``sequence fails if any of the parsers fail`` () =
        let state = StringParseState ("abc", 0)
        let actual = sequence [token 'a'; token 'x'; token 'c'] state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``sequence succeeds if all parsers succeed`` () =
        let state = StringParseState ("abc", 0)
        let actual = sequence [token 'a'; token 'b'; token 'c'] state
        let expected = Success { Consumed = true; Value = ['a'; 'b'; 'c'] }, StringParseState ("abc", 3)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``sequenceToken fails if any of the parsers fail`` () =
        let state = StringParseState ("abc", 0)
        let actual = sequenceToken ['a'; 'x'; 'c'] state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``sequenceToken succeeds if all parsers succeed`` () =
        let state = StringParseState ("abc", 0)
        let actual = sequenceToken ['a'; 'b'; 'c'] state
        let expected = Success { Consumed = true; Value = ['a'; 'b'; 'c'] }, StringParseState ("abc", 3)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``string fails if string is different`` () =
        let state = StringParseState ("abc", 0)
        let actual = string "axc" state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<string> * ParseState>(expected, actual)

    [<Fact>]
    let ``string succeeds if string matches`` () =
        let state = StringParseState ("abc", 0)
        let actual = string "abc" state
        let expected = Success { Consumed = true; Value = "abc" }, StringParseState ("abc", 3)

        Assert.Equal<Result<string> * ParseState>(expected, actual)

    [<Fact>]
    let ``map1 fails if parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = map1 Char.ToUpper (token 'x') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``map1 returns the transformed output if the parser succeeds`` () =
        let state = StringParseState ("abc", 0)
        let actual = map1 Char.ToUpper (token 'a') state
        let expected = Success { Consumed = true; Value = 'A' }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``map2 fails if the first parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = map2 (fun c1 c2 -> c2) (token 'x') (token 'b') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``map2 fails if the second parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = map2 (fun c1 c2 -> c2) (token 'a') (token 'x') state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``map2 returns the transformed output if both parsers succeeds`` () =
        let state = StringParseState ("abc", 0)
        let actual = map2 (fun c1 c2 -> c1.ToString() + c2.ToString()) (token 'a') (token 'b') state
        let expected = Success { Consumed = true; Value = "ab" }, StringParseState ("abc", 2)

        Assert.Equal<Result<string> * ParseState>(expected, actual)

    [<Fact>]
    let ``many returns success with empty result if no token matched`` () =
        let state = StringParseState ("abc", 0)
        let actual = many (token 'x') state
        let expected = Success { Consumed = false; Value = [] }, StringParseState ("abc", 0)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``many returns tokens until they match`` () =
        let state = StringParseState ("abcA", 0)
        let actual = many (tokenPred Char.IsLower) state
        let expected = Success { Consumed = true; Value = ['a'; 'b'; 'c'] }, StringParseState ("abcA", 3)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``atLeastOnce fails if no token matches`` () =
        let state = StringParseState ("abc", 0)
        let actual = atLeastOnce (token 'x') state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``atLeastOnce returns tokens until they match`` () =
        let state = StringParseState ("abcA", 0)
        let actual = atLeastOnce (tokenPred Char.IsLower) state
        let expected = Success { Consumed = true; Value = ['a'; 'b'; 'c'] }, StringParseState ("abcA", 3)

        Assert.Equal<Result<char list> * ParseState>(expected, actual)

    [<Fact>]
    let ``between fails if the the beginning fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = between (tokenPred Char.IsUpper) any any state
        let expected = Failure { Consumed = false; Message = "Not matching token." }, StringParseState ("abc", 0)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``between fails if the the end fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = between any (tokenPred Char.IsUpper) any state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 2)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``between fails if the main parser fails`` () =
        let state = StringParseState ("abc", 0)
        let actual = between any any (tokenPred Char.IsUpper) state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("abc", 1)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``between succeeds if beginning, end and the main parser succeed`` () =
        let state = StringParseState ("AbC", 0)
        let actual = between (tokenPred Char.IsUpper) (tokenPred Char.IsUpper) any state
        let expected = Success { Consumed = true; Value = 'b' }, StringParseState ("AbC", 3)

        Assert.Equal<Result<char> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated succeeds if end of input`` () =
        let state = StringParseState ("", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Success { Consumed = false; Value = [] }, StringParseState ("", 0)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated succeeds if matches once`` () =
        let state = StringParseState ("foo", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Success { Consumed = true; Value = [ "foo" ] }, StringParseState ("foo", 3)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated succeeds if matches twice`` () =
        let state = StringParseState ("foo foo", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Success { Consumed = true; Value = [ "foo"; "foo" ] }, StringParseState ("foo foo", 7)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated succeeds if matches once then no separator`` () =
        let state = StringParseState ("foobar", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Success { Consumed = true; Value = [ "foo" ] }, StringParseState ("foobar", 3)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated succeeds if first doesn't match`` () =
        let state = StringParseState ("bar", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Success { Consumed = false; Value = [ ] }, StringParseState ("bar", 0)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated fails if first partially match`` () =
        let state = StringParseState ("four", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("four", 2)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)

    [<Fact>]
    let ``separated fails if separator matches but not the next item`` () =
        let state = StringParseState ("foo bar", 0)
        let actual = separated (token ' ') (string "foo") state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("foo bar", 4)

        Assert.Equal<Result<string list> * ParseState>(expected, actual)