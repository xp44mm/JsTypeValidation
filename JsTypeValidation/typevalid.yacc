pattern: NULL
    | BOOLEAN
    | NUMBER
    | QUOTE
    | TYPE
    | ID
    | array
    | object
    | pattern "|" pattern
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
elements:              pattern
        | elements "," pattern
;
properties:                prop
          | properties "," prop
;
prop: key
    | key ":" pattern
;
key: ID
   | QUOTE
;

%%

%left "|"

