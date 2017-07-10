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
        let state = StringParseState("<foo>tagcontent</bar>")
        let actual = tag state
        let expected = Failure { Consumed = true; Message = "Not matching token." }

        Assert.Equal<Result<XmlTag>>(expected, actual)

    [<Fact>]
    let ``tagClose matches a closer tag with the matching name`` () =
        let state = StringParseState("</foo>")
        let actual = state |> tagClose "foo"
        let expected = Success { Consumed = true; Value = "foo" }

        Assert.Equal<Result<string>>(expected, actual)

    [<Fact>]
    let ``tagClose fails if the name does not match`` () =
        let state = StringParseState("</foo>")
        let actual = state |> tagClose "bar"
        let expected = Failure { Consumed = true; Message = "Not matching token." }

        Assert.Equal<Result<string>>(expected, actual)

    [<Fact>]
    let ``tagOpen matches an opening tag`` () =
        let state = StringParseState("<foo>")
        let actual = state |> tagOpen
        let expected = Success { Consumed = true; Value = "foo", [] }

        Assert.Equal<Result<string * XmlAttribute list>>(expected, actual)

    [<Fact>]
    let ``tagOpen fails if it's a closing tag`` () =
        let state = StringParseState("</foo>")
        let actual = state |> tagOpen 
        let expected = Failure { Consumed = true; Message = "Not matching token." }

        Assert.Equal<Result<string * XmlAttribute list>>(expected, actual)

    [<Fact>]
    let ``can parse a simple tag`` () =
        let state = StringParseState("<foo>bar</foo>")

        let simpleTag state =
            tagOpen
            |> bindAndTransform
                (fun (n, a) -> (rawString |> before (tagClose n)))
                (fun (n, a) c -> xmlTag n a c) <| state

        let actual = state |> simpleTag
        let expected = Success { Consumed = true; 
                                 Value = { Name = "foo" 
                                           Attributes = [] 
                                           Content = RawString "bar" } }

        Assert.Equal<Result<XmlTag>>(expected, actual)

    [<Fact>]
    let ``tag can parse XML tag with string content`` () =
        let state = StringParseState("<foo attr1=\"val1\" attr2=\"val2\">bar</foo>")
        let actual = tag state
        
        let expected = Success { Consumed = true; 
                                 Value = { Name = "foo" 
                                           Attributes = [ XmlAttribute ("attr1", "val1"); XmlAttribute ("attr2", "val2") ] 
                                           Content = RawString "bar" } }

        Assert.Equal<Result<XmlTag>>(expected, actual)

    [<Fact>]
    let ``tag can parse XML tag with empty content`` () =
        let state = StringParseState("<foo></foo>")
        let actual = tag state

        let expected = Success { Consumed = true; 
                                 Value = { Name = "foo" 
                                           Attributes = [] 
                                           Content = Empty } }

        Assert.Equal<Result<XmlTag>>(expected, actual)

    [<Fact>]
    let ``rawString fails with Consumed false for an xml tag`` () =
        let state = StringParseState("</bar>")
        let actual = rawString state

        let expected = Failure { Consumed = false; Message = "Not matching token." }

        Assert.Equal<Result<XmlTagContent>>(expected, actual)

    [<Fact>]
    let ``tag can parse XML tag with inner xml`` () =
        let state = StringParseState("<foo><bar attr1=\"val1\" attr2=\"val2\">baz</bar></foo>")
        let actual = tag state

        let expected = Success { Consumed = true;
                                 Value = { Name = "foo"
                                           Attributes = []
                                           Content = InnerXml [ { Name = "bar"
                                                                  Attributes = [ XmlAttribute ("attr1", "val1"); XmlAttribute ("attr2", "val2") ]
                                                                  Content = RawString "baz" }] } }

        Assert.Equal<Result<XmlTag>>(expected, actual)