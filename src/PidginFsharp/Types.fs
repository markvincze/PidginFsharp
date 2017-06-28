namespace PidginFsharp

type ParseError = {
    Consumed : bool
    Message : string }

type ParseSuccess<'T> = {
    Consumed : bool
    Value : 'T }

type ParseSuccess = string

type ParseState =
| StringParseState of string * int

type Result<'T> =
| Success of ParseSuccess<'T>
| Failure of ParseError

type Parse<'T> = ParseState -> Result<'T> * ParseState

module Types =
    let parseSuccess consumed value =
        Success {
            Consumed = consumed
            Value = value }

    let parseFailure consumed message =
        Failure {
            Consumed = consumed
            Message = message }