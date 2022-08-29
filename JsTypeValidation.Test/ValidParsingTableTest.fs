namespace JsTypeValidation

open Xunit
open Xunit.Abstractions
open System
open System.IO

open FSharpCompiler.Yacc
open FSharp.Literals
open FSharp.xUnit
open FSharp.Idioms

type ValidParsingTableTest(output:ITestOutputHelper) =
    let show res =
        res
        |> Render.stringify
        |> output.WriteLine

    let locatePath = Path.Combine(
                        DirectoryInfo(__SOURCE_DIRECTORY__).Parent.FullName,
                        "JsTypeValidation")
    let filePath = Path.Combine(locatePath, "typevalid.yacc")
    let yaccFile = File.ReadAllText(filePath)
    let yacc = YaccFile.parse yaccFile

    [<Fact>]
    member this.``1-input data``() =

        show yacc.mainRules
        show yacc.precedences

    [<Fact>]
    member this.``2-production Conflict``() =
        let tbl = AmbiguousTable.create yacc.mainRules
        let pconflicts = ConflictFactory.productionConflict tbl.ambiguousTable
        //show pconflicts
        Assert.True(pconflicts.IsEmpty)

    [<Fact>]
    member this.``3-operator overloads warning``() =
        let tbl = AmbiguousTable.create yacc.mainRules
        let warning = ConflictFactory.overloadsWarning tbl
        Assert.True(warning.IsEmpty)

    [<Fact>]
    member this.``4-Shift Reduce Conflict``() =
        let tbl = AmbiguousTable.create yacc.mainRules
        let srconflicts = ConflictFactory.shiftReduceConflict tbl
        //show srconflicts
        let y = set [set [["value";"value";"|";"value"]]]
        Should.equal y srconflicts

    [<Fact(Skip="once and for all!")>] // 
    member this.``5-generate parsing table``() =
        let parseTbl = ParseTable.create(yacc.mainRules, yacc.precedences)

        let result =
            [
                "module JsTypeValidation.ValidParsingTable" // {namespace}.{SematicName}ParsingTable
                "let rules = " + Literal.stringify parseTbl.rules
                "let actions = " + Literal.stringify parseTbl.actions
                "let kernelSymbols = " + Literal.stringify parseTbl.kernelSymbols
                "open FSharpCompiler.Parsing"
                "let pconfig = ParserConfig(rules, actions, kernelSymbols)"
            ] |> String.concat Environment.NewLine
        let outputDir = Path.Combine(locatePath, "ValidParsingTable.fs") // {SematicName}ParsingTable.fs
        File.WriteAllText(outputDir,result)
        output.WriteLine("the yacc has been generated: " + outputDir)

    [<Fact>]
    member this.``6-verify parsing table``() =
        let parseTbl = ParseTable.create(yacc.mainRules, yacc.precedences)
        Should.equal parseTbl.rules         ValidParsingTable.rules
        Should.equal parseTbl.actions       ValidParsingTable.actions
        Should.equal parseTbl.kernelSymbols ValidParsingTable.kernelSymbols

    [<Fact>]
    member this.``7-all of the terminals in the grammar``() =
        let grammar = Grammar.from yacc.mainRules
        let terminals = grammar.symbols - grammar.nonterminals
        let y = set [",";"...";":";"BOOLEAN";"ID";"NULL";"NUMBER";"QUOTE";"TYPE";"[";"]";"{";"|";"}"]
        Should.equal y terminals

    [<Fact>]
    member this.``9-generate translation framework``() =
        let renderToken tag lexeme = 
            match tag with
            | "|"    -> " BAR"
            | ","    -> " COMMA"
            | ":"    -> " COLON"
            | "["    -> " LBRACK"
            | "]"    -> " RBRACK"
            | "{"    -> " LBRACE"
            | "}"    -> " RBRACE"
            | "..."  -> " ELLIPSIS"
            | "NULL" -> " NULL"
            | tag    -> $"({tag} {lexeme})"

        let generate = TranslationGenerator.generateConfig renderToken
        let grammar = Grammar.from yacc.mainRules
        let code = generate grammar.nonterminals yacc.mainRules
        output.WriteLine(code)

    [<Fact>]
    member this.``a - generate json parsing table``() =
        let parseTbl = ParseTable.create(yacc.mainRules, yacc.precedences)
        let json = FSharpCompiler.Json.JsonFormatter.serialize parseTbl
        output.WriteLine(json)

    [<Fact>]
    member this.``b - generate javascript translation framework``() =
        let tokenTags =
            Map [
                "|"      , "BAR" 
                ","      , "COMMA" 
                ":"      , "COLON" 
                "["      , "LBRACK" 
                "]"      , "RBRACK" 
                "{"      , "LBRACE" 
                "}"      , "RBRACE" 
                "..."    , "ELLIPSIS" 
                "NULL"   , "NULL" 
                "TYPE"   , "TYPE" 
                "ID"     , "ID" 
                "BOOLEAN", "BOOLEAN" 
                "NUMBER" , "NUMBER" 
                "QUOTE"  , "QUOTE" 
             ]

        let semTokens = set [
            "TYPE"
            "ID"
            "BOOLEAN"
            "NUMBER"
            "QUOTE"
        ]

        let generate = TranslationGenerator.generateJavaScript tokenTags semTokens
        let grammar = Grammar.from yacc.mainRules
        let code = generate grammar.nonterminals yacc.mainRules
        output.WriteLine(code)
