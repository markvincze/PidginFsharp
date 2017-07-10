namespace PidginFsharp


type IParseState<'a> =
    abstract member Advance: unit -> unit
    abstract member Peek: unit -> 'a option
    abstract member PushBookmark: unit -> unit
    abstract member PopBookmark: unit -> unit
    abstract member Rewind: unit -> unit
    abstract member Pos: unit -> int

type StringParseState<'a>(str) =
    let mutable str = str
    let mutable pos = 0
    let bookmarks = System.Collections.Generic.Stack<int>()

    interface IParseState<char> with
        member this.Peek () =
            if pos >= 0 && pos < String.length str
            then Seq.item pos str |> Some
            else None

        member this.Advance () =
            pos <- pos + 1
        
        member this.PushBookmark () =
            bookmarks.Push(pos)
        
        member this.PopBookmark () =
            bookmarks.Pop() |> ignore
        
        member this.Rewind () =
            let oldPos = bookmarks.Pop()
            pos <- oldPos

        member this.Pos () =
            pos

module ParserState =
    let peek (state:IParseState<'T>) = state.Peek()

    let advance (state:IParseState<'T>) = state.Advance()

    let pushBookmark (state:IParseState<'T>) = state.PushBookmark()

    let popBookmark (state:IParseState<'T>) = state.PopBookmark()

    let rewind (state:IParseState<'T>) = state.Rewind()

    let pos (state:IParseState<'T>) = state.Pos()