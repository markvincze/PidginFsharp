namespace PidginFsharp

type StringParseState = {
        Str : string
        mutable Pos : int
        Bookmarks : System.Collections.Generic.Stack<int>
    } with
    member this.Peek () =
        if this.Pos >= 0 && this.Pos < String.length this.Str
        then Seq.item this.Pos this.Str |> Some
        else None

    member this.Advance () =
        this.Pos <- this.Pos + 1
    
    member this.PushBookmark () =
        this.Bookmarks.Push(this.Pos)
    
    member this.PopBookmark () =
        this.Bookmarks.Pop() |> ignore
    
    member this.Rewind () =
        let oldPos = this.Bookmarks.Pop()
        this.Pos <- oldPos

type ParserState =
| StringParserState of StringParseState

module ParserState =
    let createStringParserState str =
        {
            Str = str
            Pos = 0
            Bookmarks = System.Collections.Generic.Stack<int>()
        }
        |> StringParserState

    let peek (state:ParserState) =
        match state with
        | StringParserState state -> state.Peek()

    let advance (state:ParserState) =
        match state with
        | StringParserState state -> state.Advance()

    let pushBookmark (state:ParserState) =
        match state with
        | StringParserState state -> state.PushBookmark()

    let popBookmark (state:ParserState) =
        match state with
        | StringParserState state -> state.PopBookmark()

    let rewind (state:ParserState) =
        match state with
        | StringParserState state -> state.Rewind()