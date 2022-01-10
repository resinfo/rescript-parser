module Ast = Lang_sexp_parser__ast
module P = Parser

let literal: Lang_sexp_parser__literal.P.t<Lang_sexp_parser__literal.Ast.literal>
let definition: Lang_sexp_parser__definition.P.t<Lang_sexp_parser__definition.Ast.definition>
let parser: P.t<Ast.t>
