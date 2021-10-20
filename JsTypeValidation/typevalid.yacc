value: NULL
    | BOOLEAN
    | NUMBER
    | QUOTE
    | TYPE
    | ID
    | array
    | object
    | value "|" value
;
array : "["                                 "]"
      | "[" elements                        "]"
      | "["              "..."              "]"
      | "[" elements "," "..."              "]"
      | "["              "..." "," elements "]"
      | "[" elements "," "..." "," elements "]"
;
object : "{""}"
       | "{" properties "}"
       | "{" "..." "}"
       | "{" properties "," "..." "}"
;
elements:              value
        | elements "," value
;
properties:                prop
          | properties "," prop
;
prop: key
    | key ":" value
;
key: ID
   | QUOTE
;

%%

%left "|"

