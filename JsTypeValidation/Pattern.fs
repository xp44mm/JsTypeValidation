namespace JsTypeValidation

/// Represents a syntax tree for an pattern
type Pattern =
| Null
| BooleanOf of bool
| NumberOf of float
| QuoteOf of string
| TypeOf of string
| Wild of string
| FixedArray of Pattern list
| VariadicArray of head:Pattern list * tail:Pattern list
| ExactObject of (string*Pattern) list
| CompatObject of (string*Pattern) list
| Either of Pattern * Pattern

//| Array of ElemComp list
//| Object of PropComp list
//and ElemComp =
//| Element of Pattern
//| RestArray of string // gather

//and PropComp =
//| Prop of string * Pattern
//| RestObject of string

