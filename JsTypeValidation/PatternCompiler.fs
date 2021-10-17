module JsTypeValidation.PatternCompiler

let compile (pat:string) =
    pat
    |> PatternTokenizer.tokenize
    |> Driver.parseToTree
    |> Translation.trans_pattern