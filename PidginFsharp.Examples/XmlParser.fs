namespace PidginFsharp.Examples

module Bar =
    let bar foo = 5
// open System
// open PidginFsharp

// type XmlAttribute = XmlAttribute of string * string

// type XmlTag = {
//     Name : string
//     Content : XmlTagContent
//     Attributes : XmlAttribute list
// }
// and XmlTagContent =
// | Empty
// | RawString of string
// | InnerXml of XmlTag list

// module XmlParser =
//     open Basic
//     open Sequence

//     let xmlTag name attribs content = {
//         Name = name
//         Attributes = attribs
//         Content = content
//     }

//     let whitespace = tokenPred Char.IsWhiteSpace
//     let whitespaces = many whitespace
//     let quote = token '"'
//     let openAngle = token '<'
//     let openAngleSlash = string "</"
//     let closeAngle = token '>'

//     let attributeValue =
//         many (tokenNot '"')
//         |> select String.Concat

//     let symbolName =
//         many (tokenPred Char.IsLetterOrDigit)
//         |> select String.Concat

//     let attribute =
//         map2
//             (fun n v -> XmlAttribute (n, v))
//             (symbolName |> before (token '='))
//             (between quote quote attributeValue)

//     let tagOpen =
//         map2
//             (fun name attributes -> name, attributes)
//             (after openAngle symbolName)
//             (attribute |> separated whitespaces |> between whitespaces whitespaces)
//         |> before closeAngle

//     let tagClose name =
//         after openAngleSlash (string name)
//         |> before closeAngle
    
//     let rawString =
//         many (tokenNot '<')
//         |> select (String.Concat >> RawString)

//     // let rec tag state =
//     //     tagOpen
//     //     |> bindAndTransform
//     //         (fun (n, a) -> (symbolName |> before (tagClose n)))
//     //         (fun (n, a) v -> xmlTagWithString n a v) <| state

//     let rec tag state =
//         tagOpen
//         |> bindAndTransform
//             (fun (n, a) -> (tagContent |> before (tagClose n)))
//             (fun (n, a) c -> xmlTag n a c) <| state
//     and tagContent =
//         oneOf [ (many tag) |> (select InnerXml)
//                 rawString
//                 (value Empty) ]