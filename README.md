# PidginFsharp

F# port of the [Pidgin](https://github.com/benjamin-hodgson/Pidgin) parser combinator library.
It builds on the same primitive parsers and combinators described in the [Pidgin documentation](https://github.com/benjamin-hodgson/Pidgin#pidgin)

This port was started mainly as a practice project to learn more about F# programming, it's not intended for production use yet due to features of Pidgin still missing, and there has been no focus put on performance. If you're looking for a production-ready parser library and can use C#, then you can use the original [Pidgin](https://github.com/benjamin-hodgson/Pidgin), or if you're looking for a mature and performant library in F#, take a look at [FParsec](https://github.com/stephan-tolksdorf/fparsec).

## Examples

To illustrate the sytax being used, here are a couple of examples.

Parse a single 'a' character.

```
let parser = token 'a'
```

Parse a digit and a letter after each other.

```
let parser =
    [ tokenPred Char.IsDigit
      tokenPred Char.IsLetter ]
    |> sequence
```

Parse a list of digit characters between an opening and closing parenthesis.

```
let parser =
    tokenPred Char.IsDigit
    |> separated (token ',')
    |> between (token '(') (token ')')
```

Parse a list of integers between an opening and closing parenthesis, and actually return the parsed integer values.

```
let integer =
    tokenPred Char.IsDigit
    |> atLeastOnce
    |> select (String.Concat >> Int32.Parse)

let parser =
    integer
    |> separated (token ',')
    |> between (token '(') (token ')')
```

A recursive example (the same one from the [Pidgin docs](https://github.com/benjamin-hodgson/Pidgin#recursive-grammars)), matching a single digit between an equal number of closing and opening parantheses.

```
let rec parenthesised state =
    digitInsideParens
    |> between (token '(') (token ')') <| state
and digitInsideParens state =
    either (tokenPred Char.IsDigit) parenthesised <| state
```

You can find many more small examples in the [unit test project](/PidginFsharp.Test), and a more complex one is a simple [Json parser](/PidginFsharp.Examples/JsonParser.fs).