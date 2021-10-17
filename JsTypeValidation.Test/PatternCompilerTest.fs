namespace JsTypeValidation

open Xunit
open Xunit.Abstractions
open System
open FSharp.Literals

open FSharpCompiler.Parsing
open FSharp.xUnit
open JsTypeValidation

type PatternCompilerTest(output:ITestOutputHelper) =
    let show res =
        res
        |> Render.stringify
        |> output.WriteLine

    [<Fact>]
    member this.``wild``() =
        let x = "x"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| Wild "x"

    [<Fact>]
    member this.``null``() =
        let x = "null"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| Null

    [<Fact>]
    member this.``boolean``() =
        let x = "true|false"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| Either(BooleanOf true,BooleanOf false)

    [<Fact>]
    member this.``number``() =
        let x = "-3.14"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| NumberOf -3.14

    [<Fact>]
    member this.``string``() =
        let x = "\"\""
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| QuoteOf ""

    [<Fact>]
    member this.``typeof``() =
        let x = "string"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| TypeOf "string"

    [<Fact>]
    member this.``head name``() =
        let x = "string"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| TypeOf "string"

    [<Fact>]
    member this.``empty array``() =
        let x = "[]"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| FixedArray[]

    [<Fact>]
    member this.``any array``() =
        let x = "[...]"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y
        <| VariadicArray([],[])

    [<Fact>]
    member this.``two elems array``() =
        let x = "[a,b]"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y
        <| FixedArray([Wild "a";Wild "b"])

    [<Fact>]
    member this.``rest 4 elems array``() =
        let x = "[a,...,c,d]"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y
        <| VariadicArray([Wild "a"],[Wild "c";Wild "d"])

    [<Fact>]
    member this.``empty object``() =
        let x = "{}"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| ExactObject([])

    [<Fact>]
    member this.``any object``() =
        let x = "{...}"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| CompatObject[]

    [<Fact>]
    member this.``single id object``() =
        let x = "{a}"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| ExactObject["a", Wild "a"]

    [<Fact>]
    member this.``fields object``() =
        let x = "{a,b,...}"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| CompatObject([("a", Wild "a");("b", Wild "b")])

    [<Fact>]
    member this.``nested object``() =
        let x = "{a:{}}"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| ExactObject [("a", ExactObject[])]

    [<Fact>]
    member this.``left prec object``() =
        let x = "null|false|number"
        let y = PatternCompiler.compile x
        //show y
        Should.equal y 
        <| Either(Either(Null,BooleanOf false),TypeOf "number")

