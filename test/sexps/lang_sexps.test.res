module P = Parser
module Sexp = Lang_sexp_parser
open Ava

let file = Node.Fs.readFileAsUtf8Sync("test/sexps/inputs/valid/file.clj")

let run = P.run(Sexp.parser)

test("[Sexp lang] Module", t => {
  let input = `(module file {${file}})`

  switch run(input) {
  | Ok(
      Sexp.Module(
        "file",
        list{Sexp.DVariable("hello", Sexp.LString("world"))},
        list{("hello", Sexp.LIdentifier("hello"))},
      ),
      "",
    ) =>
    t->pass()
  | Ok(_, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
  | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
  }
})