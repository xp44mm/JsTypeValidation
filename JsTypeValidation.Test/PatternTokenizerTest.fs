namespace JsTypeValidation

open FSharpCompiler.Yacc

open Xunit
open Xunit.Abstractions
open System
open System.IO

open FSharp.Literals
open FSharp.xUnit
open JsTypeValidation

type PatternTokenizerTest(output:ITestOutputHelper) =
    let show res =
        res
        |> Render.stringify
        |> output.WriteLine

    [<Fact>]
    member this.``tryIdentifier``() =
        let x = "__"
        let y = PatternTokenizer.tryIdentifier x
        let z = Some(ID "__","")
        Should.equal y z

    [<Fact>]
    member this.``tokenize``() =
        let x = "__"
        let y = PatternTokenizer.tokenize x |> Seq.toList
        let z = [ID "__"]
        Should.equal y z
