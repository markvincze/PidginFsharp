namespace PidginFsharp.Test

open PidginFsharp
open PidginFsharp.Examples
open System
open Xunit

module XmlTests =
    open Basic
    open Sequence
    open XmlParser

    [<Fact>]
    let ``tag fails if tag names don't match`` () =
        tag |> Util.fails "<foo>tagcontent</bar>" true

    [<Fact>]
    let ``tagClose matches a closer tag with the matching name`` () =
        tagClose "foo"
        |> Util.succeed "</foo>" "foo"

    [<Fact>]
    let ``tagClose fails if the name does not match`` () =
        tagClose "bar"
        |> Util.fails "</foo>" true

    [<Fact>]
    let ``tagOpen matches an opening tag`` () =
        tagOpen |> Util.succeed "<foo>" ("foo", [])

    [<Fact>]
    let ``tagOpen fails if it's a closing tag`` () =
        tagOpen |> Util.fails "</foo>" true

    [<Fact>]
    let ``tag can parse XML tag with string content`` () =
        tag
        |> Util.succeed
            "<foo attr1=\"val1\" attr2=\"val2\">bar</foo>"
            {
                Name = "foo" 
                Attributes = [ XmlAttribute ("attr1", "val1"); XmlAttribute ("attr2", "val2") ] 
                Content = RawString "bar"
            }

    [<Fact>]
    let ``tag can parse XML tag with empty content`` () =
        tag |>
        Util.succeed
            "<foo></foo>"
            {
                Name = "foo" 
                Attributes = [] 
                Content = Empty
            }

    [<Fact>]
    let ``rawString fails with Consumed false for an xml tag`` () =
        rawString |> Util.fails "</bar>" false

    [<Fact>]
    let ``tag can parse XML tag with inner xml`` () =
        tag |>
        Util.succeed
            "<foo><bar attr1=\"val1\" attr2=\"val2\">baz</bar></foo>"
            {
                Name = "foo"
                Attributes = []
                Content = InnerXml [ { Name = "bar"
                                       Attributes = [ XmlAttribute ("attr1", "val1"); XmlAttribute ("attr2", "val2") ]
                                       Content = RawString "baz" } ]
            }