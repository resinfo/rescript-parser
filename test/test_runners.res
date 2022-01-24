open Ava

module P = Res_parser

let makeTests = (~parser: P.t<'t>, ~makeName, ~specs) => {
  specs->Belt.Array.mapWithIndex((index, spec) => (parser, makeName(index, spec), spec))
}

let runTests = (~parser, ~makeName, ~specs) => {
  let tests = makeTests(~parser, ~makeName, ~specs)

  tests->Belt.Array.forEach(((parser, name, (input, expected, remaining))) => {
    test(name, t => {
      switch P.run(parser, input) {
      | Ok(output, state) => {
          t->true_(output == expected, ())
          t->true_(state->P.State.remaining == remaining, ())
        }
      | Error(err) => t->fail(~message=`Should not fail with "${err.message}"`, ())
      }
    })
  })
}
