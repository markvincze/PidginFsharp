namespace PidginFsharp.Examples

open System
open PidginFsharp

type Json =
    | JsonString of string
    | JsonArray of Json list
    | JsonObject of Map<string, Json>

module JsonParser =
    open Basic
    open Sequence

    let whitespace state = tokenPred Char.IsWhiteSpace <| state
    let whitespaces state = many whitespace <| state
    let lbrace state = token '{' <| state
    let rbrace state = token '}' <| state
    let lbracket state = token '[' <| state
    let rbracket state = token ']' <| state
    let quote state = token '"' <| state
    let colon state = token ':' <| state
    let colonWhitespace state = between whitespaces whitespaces colon <| state
    let comma state = token ',' <| state

    let string state =
        many (tokenNot '"')
        |> between quote quote
        |> select String.Concat <| state

    let jsonString state = string |> select JsonString <| state
    
    let rec jsonRaw state =
        oneOf [ jsonString; jsonArray; jsonObject ] <| state
    and jsonArray state =
        jsonRaw
        |> between whitespaces whitespaces
        |> separated comma
        |> between lbracket rbracket
        |> select JsonArray <| state
    and jsonMember state =
        map2 (fun name value -> name, value) (before colonWhitespace string) jsonRaw <| state
    and jsonObject state =
        jsonMember
        |> between whitespaces whitespaces
        |> separated comma
        |> between lbrace rbrace
        |> select (Map.ofList >> JsonObject) <| state

    let json state = jsonRaw |> between whitespaces whitespaces <| state