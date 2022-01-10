module P = Parser
module Sexp = Lang_sexp_parser
open Ava

let run = P.run(Sexp.definition)
let successes = [
  (
    "Variable assignment literal",
    `(let hello "world")`,
    Sexp.Ast.DVariable("hello", LString("world")),
  ),
  (
    "Variable assignment lambda",
    `(let hello (lam [a b] 12))`,
    DVariable("hello", LLambda(list{"a", "b"}, LNumber("12"))),
  ),
  (
    "Variable assignment with special chars 1",
    //
    `(let _hello 12)`,
    DVariable("_hello", LNumber("12")),
  ),
  (
    "Variable assignment with special chars 2",
    //
    `(let hello-world 12)`,
    DVariable("hello-world", LNumber("12")),
  ),
  (
    "Variable assignment with special chars 3",
    //
    `(let hello---world 12)`,
    DVariable("hello---world", LNumber("12")),
  ),
  (
    "Variable assignment with special chars 4",
    //
    `(let my_thinGY 12)`,
    DVariable("my_thinGY", LNumber("12")),
  ),
  (
    "Variable assignment with special chars 5",
    //
    `(let hello-world- 12)`,
    DVariable("hello-world-", LNumber("12")),
  ),
  (
    "Function definition",
    `(fun hello [a b] 12)`,
    DFunction("hello", list{"a", "b"}, LNumber("12")),
  ),
  ("Export string", `(export hello "world")`, DExport("hello", LString("world"))),
  ("Export identifier", `(export hello world)`, DExport("hello", LIdentifier("world"))),
  (
    "Simple module",
    `
  (module name {
    (let world "hello")

    (export hello world)
  })
  `,
    DModule(
      Module(
        "name",
        list{DVariable("world", LString("hello"))},
        list{("hello", LIdentifier("world"))},
      ),
    ),
  ),
  ("Empty module", `(module foo {})`, DModule(Module("foo", list{}, list{}))),
]

successes->Belt.Array.forEach(((name, input, expected)) => {
  test(`[Sexp lang] Defintion "${name}" success`, t => {
    switch run(input) {
    | Ok(output, "") if output == expected => t->pass(~message=``, ())
    | Ok(_ast, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
    | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
    }
  })
})
