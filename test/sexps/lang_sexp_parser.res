module Ast = Lang_sexp_parser__ast
module P = Parser

let charListToString = ls =>
  ls->Belt.List.map(Char.escaped)->Belt.List.reduce("", Js.String2.concat)

let whitespace = P.anyOf([' ', '\n', '\r', '\t'])
let manyWhitespace = P.many(whitespace)
let betweenManyWhitespace = P.between(_, manyWhitespace, manyWhitespace)

let lParen = P.char('(')
let rParen = P.char(')')

let lBrace = P.char('{')
let rBrace = P.char('}')

let betweenParens = P.between(_, lParen, rParen)
let betweenBraces = P.between(_, lBrace, rBrace)

let uppercase = P.satisfy(c => c >= 'a' && 'z' >= c)
let lowercase = P.satisfy(c => c >= 'A' && 'Z' >= c)

let moduleIdentifier = {
  P.atLeastOne(P.choice([uppercase, lowercase]))->P.map(charListToString)
}

let literal = Lang_sexp_parser__literal.literal

let definition = Lang_sexp_parser__definition.definition

let parser = {
  P.string("module")
  ->betweenManyWhitespace
  ->P.keepRight(moduleIdentifier->betweenManyWhitespace)
  ->P.andThen(P.many(definition)->betweenBraces)
  ->betweenParens
  ->P.map(((name, defs)) => {
    let (exports, defs) = defs->Belt.List.partition(x => {
      switch x {
      | DExport(_) => true
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
