module JsTypeValidation.Translation

open JsTypeValidation
open FSharpCompiler.Parsing

let rec trans_pattern = function
| Interior("pattern", [Terminal NULL]) ->
    Null
| Interior("pattern", [Terminal(BOOLEAN x)]) ->
    BooleanOf x
| Interior("pattern", [Terminal(NUMBER x)]) ->
    NumberOf x
| Interior("pattern", [Terminal(QUOTE x)]) ->
    QuoteOf x
| Interior("pattern", [Terminal(TYPE x)]) ->
    TypeOf x
| Interior("pattern", [Terminal(ID x)]) ->
    Wild x
| Interior("pattern", [Interior("array",_) as array]) ->
    let array = trans_array array
    array
| Interior("pattern", [Interior("object",_) as object]) ->
    let object = trans_object object
    object
| Interior("pattern", [Interior("pattern",_) as pattern1;Terminal BAR;Interior("pattern",_) as pattern2]) ->
    let pattern1 = trans_pattern pattern1
    let pattern2 = trans_pattern pattern2
    Either(pattern1,pattern2)
| never -> failwithf "%A" <| never.firstLevel()

and trans_array = function
| Interior("array", [Terminal LBRACK;Terminal RBRACK]) ->
    FixedArray[]
| Interior("array", [Terminal LBRACK;Interior("elements",_) as elements;Terminal RBRACK]) ->
    let elements = trans_elements elements
    FixedArray(List.rev elements)
| Interior("array", [Terminal LBRACK;Terminal ELLIPSIS;Terminal RBRACK]) ->
    VariadicArray([],[])
| Interior("array", [Terminal LBRACK;Interior("elements",_) as elements;Terminal COMMA;Terminal ELLIPSIS;Terminal RBRACK]) ->
    let elements = trans_elements elements
    VariadicArray(List.rev elements,[])
| Interior("array", [Terminal LBRACK;Terminal ELLIPSIS;Terminal COMMA;Interior("elements",_) as elements;Terminal RBRACK]) ->
    let elements = trans_elements elements
    VariadicArray([],List.rev elements)
| Interior("array", [Terminal LBRACK;Interior("elements",_) as elements1;Terminal COMMA;Terminal ELLIPSIS;Terminal COMMA;Interior("elements",_) as elements2;Terminal RBRACK]) ->
    let elements1 = trans_elements elements1
    let elements2 = trans_elements elements2
    VariadicArray(List.rev elements1,List.rev elements2)
| never -> failwithf "%A" <| never.firstLevel()

and trans_object = function
| Interior("object", [Terminal LBRACE;Terminal RBRACE]) ->
    ExactObject []
| Interior("object", [Terminal LBRACE;Interior("properties",_) as properties;Terminal RBRACE]) ->
    let properties = trans_properties properties
    ExactObject(List.rev properties)
| Interior("object", [Terminal LBRACE;Terminal ELLIPSIS;Terminal RBRACE]) ->
    CompatObject []
| Interior("object", [Terminal LBRACE;Interior("properties",_) as properties;Terminal COMMA;Terminal ELLIPSIS;Terminal RBRACE]) ->
    let properties = trans_properties properties
    CompatObject(List.rev properties)
| never -> failwithf "%A" <| never.firstLevel()

and trans_elements = function
| Interior("elements", [Interior("pattern",_) as pattern]) ->
    let pattern = trans_pattern pattern
    [pattern]
| Interior("elements", [Interior("elements",_) as elements;Terminal COMMA;Interior("pattern",_) as pattern]) ->
    let elements = trans_elements elements
    let pattern = trans_pattern pattern
    pattern::elements
| never -> failwithf "%A" <| never.firstLevel()

and trans_properties = function
| Interior("properties", [Interior("prop",_) as prop]) ->
    let prop = trans_prop prop
    [prop]
| Interior("properties", [Interior("properties",_) as properties;Terminal COMMA;Interior("prop",_) as prop]) ->
    let properties = trans_properties properties
    let prop = trans_prop prop
    prop::properties
| never -> failwithf "%A" <| never.firstLevel()

and trans_prop = function
| Interior("prop", [Interior("key",_) as key]) ->
    let key = trans_key key
    (key,Wild key)
| Interior("prop", [Interior("key",_) as key;Terminal COLON;Interior("pattern",_) as pattern]) ->
    let key = trans_key key
    let pattern = trans_pattern pattern
    (key,pattern)
| never -> failwithf "%A" <| never.firstLevel()

and trans_key = function
| Interior("key", [Terminal(ID x)]) ->
    x
| Interior("key", [Terminal(QUOTE x)]) ->
    x
| never -> failwithf "%A" <| never.firstLevel()