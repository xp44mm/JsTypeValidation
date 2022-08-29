module JsTypeValidation.PatternTokenizer

open FSharp.Idioms
open System.Text.RegularExpressions
open System

//An js identifier must start with $, _, or any character in the Unicode categories 
//“Uppercase letter (Lu)”, “Lowercase letter (Ll)”, “Titlecase letter (Lt)”, “Modifier letter (Lm)”, “Other letter (Lo)”, 
//or “Letter number (Nl)”.

//The rest of the string can contain the same characters, plus any U+200C zero width non-joiner characters, 
//U+200D zero width joiner characters, and characters in the Unicode categories 
//“Non-spacing mark (Mn)”, “Spacing combining mark (Mc)”, “Decimal digit number (Nd)”, or “Connector punctuation (Pc)”.

let tryIdentifier = 
    //故意从标识符允许字符集中排除掉`$`
    let re = Regex "^[_\p{L}\p{Nl}][\p{L}\p{Mn}\p{Mc}\p{Nl}\p{Nd}\p{Pc}\p{Cf}]*"
    tryMatch re
    >> Option.map(fun(lex,rest)->
        let token =
            match lex with
            | "null" -> NULL
            | "true"  -> BOOLEAN true
            | "false" -> BOOLEAN false
            | "boolean" | "string" | "number" | "function" -> TYPE lex
            | _ -> ID lex
        token,rest
    )

/// in the JSON format 
let tryQuoteString =
    let re = Regex """^("(\\u[0-9A-Fa-f]{4}|\\[\\"bfnrt]|[^\\"\r\n])*")"""
    tryMatch re

/// in the JSON format 
let tryNumber =
    let re = Regex @"^[-+]?\d+(\.\d+)?([eE][-+]?\d+)?"
    tryMatch re


let tryEllipsis = tryStart "..."

let tokenize(inp:string) =
    let rec loop (inp:string) =
        seq {
            match inp with
            | "" -> ()
    
            | Prefix @"\s+" (_,rest) -> 
                yield! loop rest

            | PrefixChar '|' rest ->
                yield BAR
                yield! loop rest

            | PrefixChar ',' rest ->
                yield COMMA
                yield! loop rest

            | PrefixChar ':' rest ->
                yield COLON
                yield! loop rest

            | PrefixChar '[' rest ->
                yield LBRACK
                yield! loop rest

            | PrefixChar ']' rest ->
                yield RBRACK
                yield! loop rest

            | PrefixChar '{' rest ->
                yield LBRACE
                yield! loop rest

            | PrefixChar '}' rest ->
                yield RBRACE
                yield! loop rest

            | On tryEllipsis rest ->
                yield ELLIPSIS
                yield! loop rest

            | On tryIdentifier (token,rest) ->
                yield token
                yield! loop rest

            | On tryQuoteString (lexeme,rest) ->
                yield  QUOTE(Quotation.unquote lexeme)
                yield! loop rest

            | On tryNumber (lexeme,rest) ->
                yield  NUMBER(Double.Parse(lexeme))
                yield! loop rest

            | never -> failwith never
        }
    
    loop inp
