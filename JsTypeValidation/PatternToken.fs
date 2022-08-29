namespace JsTypeValidation

type PatternToken =
| BAR
| COMMA
| COLON
| LBRACK
| RBRACK
| LBRACE
| RBRACE
| ELLIPSIS
| NULL
| TYPE of string
| ID of string
| BOOLEAN of bool
| NUMBER of float
| QUOTE of string

    member this.getTag() =
        match this with
        | BAR      -> "|"
        | COMMA    -> ","
        | COLON    -> ":"
        | LBRACK   -> "["
        | RBRACK   -> "]"
        | LBRACE   -> "{"
        | RBRACE   -> "}"
        | ELLIPSIS -> "..."
        | NULL     -> "NULL"
        | TYPE    _ -> "TYPE"
        | ID      _ -> "ID"
        | BOOLEAN _ -> "BOOLEAN"
        | NUMBER  _ -> "NUMBER"
        | QUOTE   _ -> "QUOTE"
        
