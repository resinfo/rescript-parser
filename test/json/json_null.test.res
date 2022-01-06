open Ava
module P = Parser

let run = P.run(Json.json)

let validNulls = [
  //
  ("1", "null"),
  ("2", "     null"),
  ("3", "     null           "),
  ("4", "null           "),
]

validNulls->Belt.Array.forEach(((index, input)) => {
  test(`[JSON] Valid "null" ${index}`, t => {
    switch run(input) {
    | Ok((Null, "")) => t->pass()
    | Ok((Null, rest)) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
    | Ok((ast, _)) => t->fail(~message=`Shouldn't succeed with "${Json.toString(ast)}"`, ())
    | Error(error) => t->fail(~message=`Shouldn't fail with: "${error}"`, ())
    }
  })
})

let partiallyValidNulls = [
  //
  ("1", "nulla", "a"),
  ("2", "     null   ___", "___"),
  ("3", "     null}[]", "}[]"),
  ("4", "null           +", "+"),
]

partiallyValidNulls->Belt.Array.forEach(((index, input, remaining)) => {
  test(`[JSON] Partially valid "null" ${index}`, t => {
    switch run(input) {
    | Ok((Null, rest)) if rest == remaining => t->pass()
    | Ok((ast, rest)) =>
      t->fail(~message=`Shouldn't succeed with "${ast->Json.toString}" and "${rest}" remaining`, ())
    | Error(error) => t->fail(~message=`Shouldn't fail with: "${error}"`, ())
    }
  })
})

let invalid = [
  //
  ("1", "."),
  ("2", "-null"),
  ("3", "     ]null}[]"),
  ("4", "tnull"),
]

invalid->Belt.Array.forEach(((index, input)) => {
  test(`[JSON] Invalid "null" ${index}`, t => {
    switch run(input) {
    | Ok((ast, rest)) =>
      t->fail(~message=`Shouldn't succeed with "${ast->Json.toString}" and "${rest}" remaining`, ())
    | Error(error) => t->pass(~message=`Should fail with: "${error}"`, ())
    }
  })
})
