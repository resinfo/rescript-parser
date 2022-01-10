module Ast = Lang_sexp_parser__ast
module P = Parser

let charListToString = ls =>
  ls->Belt.List.map(Char.escaped)->Belt.List.reduce("", Js.String2.concat)

let whitespace = P.anyOf([' ', '\n', '\r', '\t'])
let manyWhitespace = P.many(whitespace)
let betweenManyWhitespace = P.between(_, manyWhitespace, manyWhitespace)

let lParen = P.char('(')
let rParen = P.char(')')
let lBracket = P.char('[')
let rBracket = P.char(']')
let lBrace = P.char('{')
let rBrace = P.char('}')

let betweenBrackets = P.between(_, lBracket, rBracket)
let betweenParens = P.between(_, lParen, rParen)
let betweenBraces = P.between(_, lBrace, rBrace)

let hyphen = P.char('-')
let underbar = P.char('_')

let digit = P.satisfy(c => c >= '0' && '9' >= c)
let uppercase = P.satisfy(c => c >= 'a' && 'z' >= c)
let lowercase = P.satisfy(c => c >= 'A' && 'Z' >= c)

let identifier = {
  let withoutLeadingHyphen =
    P.atLeastOne(P.choice([uppercase, lowercase, underbar]))
    ->P.andThen(P.many(P.choice([uppercase, lowercase, digit, hyphen, underbar])))
    ->P.map(((a, b)) => Belt.List.concat(a, b))

  withoutLeadingHyphen->P.map(charListToString)
}

let literal = Lang_sexp_parser__literal.literal

let definition = P.makeRecursive(def => {
  let export_ = {
    P.string("export")
    ->betweenManyWhitespace
    ->P.keepRight(identifier->betweenManyWhitespace)
    ->P.andThen(literal)
    ->betweenParens
  }

  let variable = {
    P.string("let")
    ->betweenManyWhitespace
    ->P.keepRight(identifier->betweenManyWhitespace)
    ->P.andThen(literal)
    ->betweenParens
  }

  let fn = {
    open Parser
    let keyword = string("fun")->betweenManyWhitespace
    let name = identifier->betweenManyWhitespace
    let arg = identifier->betweenManyWhitespace
    let args = many(arg)->betweenBrackets->betweenManyWhitespace

    keyword->keepRight(name)->andThen(args)->andThen(literal)->betweenParens
  }

  let module_ = {
    P.string("module")
    ->betweenManyWhitespace
    ->P.keepRight(identifier->betweenManyWhitespace)
    ->P.andThen(P.many(def)->betweenBraces)
    ->betweenParens
  }

  let moduleDef = {
    module_->P.map(((name, defs)) => {
      let (exports, defs) = defs->Belt.List.partition(x => {
        switch x {
        | Ast.DExport(_) => true
        | _ => false
        }
      })
      let exports = exports->Belt.List.keepMap(x => {
        switch x {
        | DExport(name, literal) => Some(name, literal)
        | _ => None
        }
      })

      Ast.Module(name, defs, exports)
    })
  }

  P.choice([
    variable->P.map(((identifier, literal)) => Ast.DVariable(identifier, literal)),
    fn->P.map((((name, args), body)) => Ast.DFunction(name, args, body)),
    moduleDef->P.map(m => Ast.DModule(m)),
    export_->P.map(((name, literal)) => Ast.DExport(name, literal)),
  ])->betweenManyWhitespace
})
