namespace PidginFsharp

open Basic
open Sequence
open System

type Json =
    | JsonString of string
    | JsonArray of Json list
    | JsonObject of Map<string, Json>

module JsonParser =

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
        |> between (token '"') (token '"')
        |> select String.Concat

    let jsonString = Basic.select JsonString string
    
    let rec json state =
        oneOf [jsonString; jsonArray] <| state
    and jsonArray state =
        json
        |> between lbracket rbracket
        |> select (fun j -> JsonArray [ j ]) <| state