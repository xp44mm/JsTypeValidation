module JsTypeValidation.Translation

open JsTypeValidation
open FSharpCompiler.Parsing
open FSharpCompiler.Parsing.ParseTreeUtils

let rec trans_value = function
    | [Terminal NULL] ->
        Null
    | [Terminal(BOOLEAN x)] ->
        BooleanOf x
    | [Terminal(NUMBER x)] ->
        NumberOf x
    | [Terminal(QUOTE x)] ->
        QuoteOf x
    | [Terminal(TYPE x)] ->
        TypeOf x
    | [Terminal(ID x)] ->
        Wild x
    | [Interior("array",array)] ->
        let array = trans_array array
        array
    | [Interior("object", object)] ->
        let object = trans_object object
        object
    | [Interior("value", value1);Terminal BAR;Interior("value", value2)] ->
        let value1 = trans_value value1
        let value2 = trans_value value2
        Either(value1,value2)
    | never -> failwithf "trans_value: %A" (List.map firstLevel never)

and trans_array = function
    | [Terminal LBRACK;Terminal RBRACK] ->
        FixedArray[]
    | [Terminal LBRACK;Interior("elements",elements);Terminal RBRACK] ->
        let elements = trans_elements elements
        FixedArray(List.rev elements)
    | [Terminal LBRACK;Terminal ELLIPSIS;Terminal RBRACK] ->
        VariadicArray([],[])
    | [Terminal LBRACK;Interior("elements",elements);Terminal COMMA;Terminal ELLIPSIS;Terminal RBRACK] ->
        let elements = trans_elements elements
        VariadicArray(List.rev elements,[])
    | [Terminal LBRACK;Terminal ELLIPSIS;Terminal COMMA;Interior("elements",elements);Terminal RBRACK] ->
        let elements = trans_elements elements
        VariadicArray([],List.rev elements)
    | [Terminal LBRACK;Interior("elements", elements1);Terminal COMMA;Terminal ELLIPSIS;Terminal COMMA;Interior("elements", elements2);Terminal RBRACK] ->
        let elements1 = trans_elements elements1
        let elements2 = trans_elements elements2
        VariadicArray(List.rev elements1,List.rev elements2)
    | never -> failwithf "trans_array: %A" (List.map firstLevel never)

and trans_object = function
    | [Terminal LBRACE;Terminal RBRACE] ->
        ExactObject []
    | [Terminal LBRACE;Interior("properties",properties);Terminal RBRACE] ->
        let properties = trans_properties properties
        ExactObject(List.rev properties)
    | [Terminal LBRACE;Terminal ELLIPSIS;Terminal RBRACE] ->
        CompatObject []
    | [Terminal LBRACE;Interior("properties", properties);Terminal COMMA;Terminal ELLIPSIS;Terminal RBRACE] ->
        let properties = trans_properties properties
        CompatObject(List.rev properties)
    | never -> failwithf "trans_object: %A" (List.map firstLevel never)

and trans_elements = function
    | [Interior("value", value)] ->
        let value = trans_value value
        [value]
    | [Interior("elements", elements);Terminal COMMA;Interior("value", value) ] ->
        let elements = trans_elements elements
        let value = trans_value value
        value::elements
    | never -> failwithf "trans_elements: %A" (List.map firstLevel never)

and trans_properties = function
    | [Interior("prop", prop)] ->
        let prop = trans_prop prop
        [prop]
    | [Interior("properties", properties);Terminal COMMA;Interior("prop", prop)] ->
        let properties = trans_properties properties
        let prop = trans_prop prop
        prop::properties
    | never -> failwithf "trans_properties: %A" (List.map firstLevel never)

and trans_prop = function
    | [Interior("key", key)] ->
        let key = trans_key key
        (key,Wild key)
    | [Interior("key", key);Terminal COLON;Interior("value", value)] ->
        let key = trans_key key
        let value = trans_value value
        (key,value)
    | never -> failwithf "trans_prop: %A" (List.map firstLevel never)

and trans_key = function
    | [Terminal(ID x)] ->
        x
    | [Terminal(QUOTE x)] ->
        x
    | never -> failwithf "trans_key: %A" (List.map firstLevel never)

let translate = function 
    | Interior("value", value) -> trans_value value 
    | root -> failwithf "translate: %A" (firstLevel root)