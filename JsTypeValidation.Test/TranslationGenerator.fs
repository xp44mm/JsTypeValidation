module FSharpCompiler.Yacc.TranslationGenerator

let generateJavaScript 
    (tokenTags: Map<string,string>) 
    (semTokens: string Set) 
    (nonterminals: string Set) 
    (productions: string list list) =

    let renderComp (comp: string) =
        if nonterminals.Contains comp then
            $"prod('{comp}')"
        else
            $"token('{tokenTags.[comp]}')"

    let renderPattern (prod: string list) =
        let components = prod.Tail

        let body =
            components
            |> List.map renderComp
            |> String.concat ","

        let definitions =
            components
            |> List.mapi(fun i c -> i, c)
            |> List.choose(fun(i, comp) -> 
                if semTokens.Contains comp then
                    Some $"ss[{i}].token.{comp}"
                elif nonterminals.Contains comp then
                    Some $"trans_{comp}(ss[{i}].children)"
                else None
                |> Option.map(fun rs -> $"let s{i} = {rs}")
            )

        [
            yield $"[rightside({body}), ss => {{"
            yield! definitions
            yield "return {}"
            yield "}],"
        ]
        |> String.concat "\n"

    let renderGroup (head:string, prods:string list list) =
        [
            yield $"const trans_{head} = cond(["
            for prod in prods do
                yield renderPattern prod
            yield $"ss => {{ throw new Error('trans_{head}') }}"
            yield "])"
            yield ""
        ]
        |> String.concat "\n"

    let renderMain (startSymbol:string) =
        [
            yield "export const translate = cond(["
            yield $"[isProd('{startSymbol}'), parseTree => trans_{startSymbol}(parseTree.children)],"
            yield "parseTree => {"
            yield "console.log(parseTree)"
            yield "throw new Error('translate')"
            yield "}"
            yield "])"
            yield ""
        ]
        |> String.concat "\n"

    let groups = productions |> List.groupBy(fun prod -> prod.Head)

    let code =
        [
            yield "import { token, prod, rightside, isProd } from './parseTree'"
            yield "import { cond } from '../ramda/cond'"
            yield ""
            yield renderMain (fst groups.Head)
            yield! groups |> List.map(renderGroup)
        ]
        |> String.concat "\n"

    code
