module P = Parser
open Ava

module Simple = Simple_sexp_parser

test("[Sexps] Char list to string", t => {
  t->is(list{'h', 'e', 'l', 'l', 'o'}->Simple.charListToString, "hello", ())
})

let run = P.run(Simple.parser)

let okStringTests = [
  //
  ("Empty", `""`, ""),
  ("Simple", `"hello"`, "hello"),
  ("With spaces", `    "  he ll   o  "`, "  he ll   o  "),
]

okStringTests->Belt.Array.forEach(((name, input, expected)) => {
  test(`[Sexps.Simple] ${name} String`, t => {
    switch run(input) {
    | Ok(Simple.String(output), "") if output == expected => t->pass()
    | Ok(_output, rest) => t->fail(~message=`Shouldn't succeed with "" and "${rest}" remaining`, ())
    | Error(err) => t->fail(~message=`Shouldn't failed with "${err}"`, ())
    }
  })
})

let okIntTests = [
  ("Simple", `1`, "1"),
  ("Simple with spaces", `    4`, "4"),
  ("More than 9", `192`, "192"),
  ("More than 9 with spaces", `  8671109    `, "8671109"),
  ("Negative", `-991344`, "-991344"),
  ("Negative with spaces", `    -9     `, "-9"),
]

okIntTests->Belt.Array.forEach(((name, input, expected)) => {
  test(`[Sexps.Simple] ${name} Int`, t => {
    switch run(input) {
    | Ok(Simple.Int(output), "") if output == expected => t->pass()
    | Ok(_output, rest) => t->fail(~message=`Shouldn't succeed with "" and "${rest}" remaining`, ())
    | Error(err) => t->fail(~message=`Shouldn't failed with "${err}"`, ())
    }
  })
})

let okExpTests = [
  ("Empty", `()`, Simple.Exp(list{})),
  ("Empty with spaces", `( )`, Exp(list{})),
  ("Simple", `(1)`, Exp(list{Int("1")})),
  (
    "Simple with spaces",
    ` (       1 )    
  
  `,
    Exp(list{Int("1")}),
  ),
  ("Multiple", `(1 "2" 3)`, Exp(list{Int("1"), String("2"), Int("3")})),
  ("Multiple with spaces", `    (  1  "2"     3  )  `, Exp(list{Int("1"), String("2"), Int("3")})),
  (
    "Nested",
    `(1 ("2" 3))`,
    Exp(list{
      Int("1"),
      Exp(list{
        //
        String("2"),
        Int("3"),
      }),
    }),
  ),
  (
    "Nested with spaces",
    `
  (     1("2"3) )       `,
    Exp(list{Int("1"), Exp(list{String("2"), Int("3")})}),
  ),
  (
    "Deeply nested with spaces",
    `
  (     1("2"(3 (1 (())))) )       `,
    Exp(list{
      Int("1"),
      Exp(list{
        String("2"),
        Exp(list{
          Int("3"),
          Exp(list{
            Int("1"),
            Exp(list{
              //
              Exp(list{}),
            }),
          }),
        }),
      }),
    }),
  ),
]

okExpTests->Belt.Array.forEach(((name, input, expected)) => {
  test(`[Sexps.Simple] ${name} Exp`, t => {
    switch run(input) {
    | Ok(output, "") if output == expected => t->pass()
    | Ok(_output, rest) => t->fail(~message=`Shouldn't succeed with "" and "${rest}" remaining`, ())
    | Error(err) => t->fail(~message=`Shouldn't failed with "${err}"`, ())
    }
  })
})
