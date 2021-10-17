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
| TYPE of string
| ID of string
| NULL
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
        | TYPE       _ -> "TYPE"
        | ID _ -> "ID"
        | NULL     -> "NULL"
        | BOOLEAN    _ -> "BOOLEAN"
        | NUMBER     _ -> "NUMBER"
        | QUOTE      _ -> "QUOTE"
        
