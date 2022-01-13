open Ava

module P = Res_parser

let run = P.run(Json.parse)

let tests = [
  //
  (`"1"`, "1"),
  (`"hello"`, "hello"),
  (`"   "`, "   "),
  (`""`, ""),
  (`"\u0050"`, "P"),
  (`"\u0050\u0069\u0061n\u006F"`, "Piano"),
]

tests->Belt.Array.forEach(((input, expected)) => {
  test(`[JSON String] "${input}" succeeds`, t => {
    switch run(input) {
    | Ok(String(x), "") if x == expected => t->pass()
    | Ok(ast, rest) =>
      t->fail(
        ~message=`Should not succeed with "${ast->Json.toString}" and "${rest}" remaining`,
        (),
      )
    | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
    }
  })
})
