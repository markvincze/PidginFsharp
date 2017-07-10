namespace PidginFsharp

type ParseError = {
    Consumed : bool
    Message : string }

type ParseSuccess<'T> = {
    Consumed : bool
    Value : 'T }

type ParseSuccess = string

type IParseState<'a> =
    abstract member Advance: unit -> unit
    
    abstract member Peek: unit -> 'a option

type StringParseState<'a>(str) =
    let mutable str = str
    let mutable pos = 0

    interface IParseState<char> with
        member this.Peek () =
            if pos >= 0 && pos < String.length str
            then Seq.item pos str |> Some
            else None

        member this.Advance () =
            pos <- pos + 1

type Result<'T> =
| Success of ParseSuccess<'T>
| Failure of ParseError

type Parse<'T> = IParseState<'T> -> Result<'T>

module Types =
    let parseSuccess consumed value =
        Success {
            Consumed = consumed
            Value = value }

    let parseFailure consumed message =
        Failure {
            Consumed = consumed
            Message = message }