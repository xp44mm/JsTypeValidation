module JsTypeValidation.Driver

// 解析tokens得到语法树
let parseToTree (tokens:seq<PatternToken>) = ValidParsingTable.pconfig.parse(tokens, fun tok -> tok.getTag())

