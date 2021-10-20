module JsTypeValidation.PatternCompiler
open FSharpCompiler.Parsing

let compile (pat:string) =
    pat
    |> PatternTokenizer.tokenize
    |> Driver.parseToTree
    |> Translation.translate