namespace JsTypeValidation

open Xunit
open Xunit.Abstractions
open System
open FSharp.Literals

open FSharpCompiler.Parsing
open FSharp.xUnit

type ParseTreeTest(output:ITestOutputHelper) =
    let show res =
        res
        |> Render.stringify
        |> output.WriteLine

    // inp -> ParseTree
    let parse(text:string) =
        let tokens = PatternTokenizer.tokenize text
        Driver.parseToTree tokens

    [<Fact>]
    member this.``value NULL``() =
        let x = "null"
        let y = parse x
        //show y
        Should.equal y 
        <| Interior("value",[
            Terminal(NULL)
            ])



    [<Fact>]
    member this.``single id``() =
        let x = "x"
        let y = parse x
        //show y
        Should.equal y 
        <| Interior("value",[
            Terminal(ID "x")
            ])

    [<Fact>]
    member this.``empty array``() =
        let x = "[]"
        let y = parse x
        //show y
        Should.equal y 
        <| Interior("value",[
            Interior("array",[Terminal LBRACK;Terminal RBRACK])
            ])

    [<Fact>]
    member this.``rest array``() =
        let x = "[...]"
        let y = parse x
        //show y
        Should.equal y
        <| Interior("value",[
            Interior("array",[Terminal LBRACK;Terminal ELLIPSIS;Terminal RBRACK])
            ])

    [<Fact>]
    member this.``empty object``() =
        let x = "{}"
        let y = parse x
        //show y
        Should.equal y 
        <| Interior("value",[
            Interior("object",[Terminal LBRACE;Terminal RBRACE])
            ])

    [<Fact>]
    member this.``single id object``() =
        let x = "{a}"
        let y = parse x
        //show y
        Should.equal y 
        <| Interior("value",[
            Interior("object",[Terminal LBRACE;Interior("properties",[Interior("prop",[Interior("key",[Terminal(ID "a")])])]);Terminal RBRACE])
            ])


