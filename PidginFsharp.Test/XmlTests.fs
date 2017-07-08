namespace PidginFsharp.Test

open PidginFsharp
open PidginFsharp.Examples
open System
open Xunit

module XmlTests =
    open XmlParser

    [<Fact>]
    let ``tag fails if tag names don't match`` () =
        let state = StringParseState ("<foo>tagcontent</bar>", 0)
        let actual = tag state
        let expected = Failure { Consumed = true; Message = "Not matching token." }, StringParseState ("<foo>tagcontent</bar>", 17)

        Assert.Equal<Result<XmlTag> * ParseState>(expected, actual)

    [<Fact>]
    let ``tag can parse XML tag`` () =
        let state = StringParseState ("<foo a1=\"v1\" a2=\"v2\">tagcontent</foo>", 0)
        let actual = tag state
        let expected =
            Success { Consumed = true;
                      Value = { Name = "foo"
                                Attributes = [ XmlAttribute ("a1", "v1")
                                               XmlAttribute ("a2", "v2") ]
                                Content = RawString "tagcontent" } },
            StringParseState ("<foo a1=\"v1\" a2=\"v2\">tagcontent</foo>", 37)

        Assert.Equal<Result<XmlTag> * ParseState>(expected, actual)

    [<Fact>]
    let ``tag can parse XML tag in empty content`` () =
        let state = StringParseState ("<foo></foo>", 0)
        let actual = tag state
        let expected =
            Success { Consumed = true;
                      Value = { Name = "foo"
                                Attributes = List.empty
                                Content = Empty } },
            StringParseState ("<foo></foo>", 11)

        Assert.Equal<Result<XmlTag> * ParseState>(expected, actual)

    [<Fact>]
    let ``tag can parse XML with inner XML`` () =
        let state = StringParseState ("<foo><bar></bar></foo>", 0)
        let actual = tag state
        let expected =
            Success { Consumed = true;
                      Value = { Name = "foo"
                                Attributes = List.empty
                                Content = InnerXml [ { Name = "bar"
                                                       Attributes = List.empty
                                                       Content = Empty } ] } },
            StringParseState ("<foo><bar></bar></foo>", 22)

        Assert.Equal<Result<XmlTag> * ParseState>(expected, actual)