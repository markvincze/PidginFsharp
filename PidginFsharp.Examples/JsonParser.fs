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

    let whitespace = tokenPred Char.IsWhiteSpace
    let whitespaces = many whitespace
    let lbrace = token '{'
    let rbrace = token '}'
    let lbracket = token '['
    let rbracket = token ']'
    let quote = token '"'
    let colon = token ':'
    let colonWhitespace = between whitespaces whitespaces colon
    let comma = token ','

    let string =
        many (tokenNot '"')
        |> between quote quote
        |> select String.Concat

    let jsonString = string |> select JsonString
    
    let rec jsonRaw state =
        oneOf [ jsonString; jsonArray; jsonObject ] <| state
    and jsonArray state =
        jsonRaw
        |> between whitespaces whitespaces
        |> separated comma
        |> between lbracket rbracket
        |> select JsonArray <| state
    and jsonMember state =
        map2 (fun name value -> name, value) (before string colonWhitespace) jsonRaw <| state
    and jsonObject state =
        jsonMember
        |> between whitespaces whitespaces
        |> separated comma
        |> between lbrace rbrace
        |> select (Map.ofList >> JsonObject) <| state

    let json = jsonRaw |> between whitespaces whitespaces