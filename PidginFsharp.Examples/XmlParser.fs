namespace PidginFsharp.Examples

module Bar =
    let bar foo = 5
open System
open PidginFsharp

type XmlAttribute = XmlAttribute of string * string

type XmlTag = {
    Name : string
    Content : XmlTagContent
    Attributes : XmlAttribute list
}
and XmlTagContent =
| Empty
| RawString of string
| InnerXml of XmlTag list

module XmlParser =
    open Basic
    open Sequence
    
    let xmlTag name attribs content = {
        Name = name
        Attributes = attribs
        Content = content
    }

    let whitespace state = tokenPred Char.IsWhiteSpace <| state
    let whitespaces state = many whitespace <| state
    let quote state = token '"' <| state
    let openAngle state = token '<' <| state
    let openAngleSlash state = string "</" <| state
    let closeAngle state = token '>' <| state

    let attributeValue state =
        many (tokenNot '"')
        |> select String.Concat <| state

    let symbolName state =
        many (tokenPred Char.IsLetterOrDigit)
        |> select String.Concat <| state

    let attribute state =
        map2
            (fun n v -> XmlAttribute (n, v))
            (symbolName |> before (token '='))
            (between quote quote attributeValue) <| state

    let tagOpen state =
        map2
            (fun name attributes -> name, attributes)
            (after openAngle symbolName)
            (attribute |> separated whitespaces |> between whitespaces whitespaces)
        |> before closeAngle <| state

    let tagClose name state =
        string name
        |> between openAngleSlash closeAngle <| state
    
    let rawString state =
        atLeastOnce (tokenNot '<')
        |> select (String.Concat >> RawString) <| state

    let rec tag state =
        tagOpen
        |> bindAndTransform
            (fun (n, a) -> (tagContent |> before (tagClose n)))
            (fun (n, a) c -> xmlTag n a c) <| state
    and tagContent state =
        oneOf [ (atLeastOnce (probe tag)) |> (select InnerXml)
                (rawString)
                (value Empty) ] <| state