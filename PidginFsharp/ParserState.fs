namespace PidginFsharp

module ParserState =
    let peek (state:IParseState<'T>) = state.Peek()

    let advance (state:IParseState<'T>) = state.Advance()