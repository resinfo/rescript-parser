open Ava
module P = Parser

@scope("JSON") @val
external stringify: 't => string = "stringify"

let run = P.run(Json.parse)

let okInputs = [
  ("Empty Object", "{}", Json.Object(list{})),
  ("Empty Object with padding", "    {  }       ", Json.Object(list{})),
  ("Object with number attribute", ` { "a":1  }       `, Json.Object(list{("a", Number("1"))})),
  ("Object with empty key", ` { "":""  }  `, Json.Object(list{("", String(""))})),
  (
    "Nested Object",
    ` { "foo": {"bar  "   : { "baz":1 }}  }  `,
    Json.Object(list{("foo", Object(list{("bar  ", Object(list{("baz", Number("1"))}))}))}),
  ),
  (
    "Object with array values 1",
    ` { "foo": [1]  }  `,
    Object(list{("foo", Array(list{Number("1")}))}),
  ),
  (
    "Object with nested array values 1",
    ` { "foo": [1, [[2]]]  }  `,
    Object(list{("foo", Array(list{Number("1"), Array(list{Array(list{Number("2")})})}))}),
  ),
  (
    "Object with nested array values 2",
    ` { "foo": [1, [[2, 2]]]  }  `,
    Object(list{
      ("foo", Array(list{Number("1"), Array(list{Array(list{Number("2"), Number("2")})})})),
    }),
  ),
  (
    "Object with nested array values 3",
    ` { "foo": [1, [[2, 2, []]]]  }  `,
    Object(list{
      (
        "foo",
        Array(list{Number("1"), Array(list{Array(list{Number("2"), Number("2"), Array(list{})})})}),
      ),
    }),
  ),
  (
    "Object with nested array values 4",
    ` { "foo": [1, [[2   ,2,[[3         ]]] ]]  }  `,
    Object(list{
      (
        "foo",
        Array(list{
          Number("1"),
          Array(list{Array(list{Number("2"), Number("2"), Array(list{Array(list{Number("3")})})})}),
        }),
      ),
    }),
  ),
  (
    "Object with nested arrays and objects",
    ` { "foo": [1, [[2   ,2,[[3        , {"b_": [1234]} ]]] ]]  }  `,
    Object(list{
      (
        "foo",
        Array(list{
          Number("1"),
          Array(list{
            Array(list{
              Number("2"),
              Number("2"),
              Array(list{
                Array(list{Number("3"), Object(list{("b_", Array(list{Number("1234")}))})}),
              }),
            }),
          }),
        }),
      ),
    }),
  ),
]

okInputs->Belt.Array.forEach(((name, input, expected)) => {
  test(`[JSON] ${name} succeeds`, t => {
    switch run(input) {
    | Ok(out, "") if out == expected => t->pass()
    | Ok(_, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
    | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
    }
  })
})

let partiallyOkInputs = [
  ("Dangling char", "{}a", Json.Object(list{}), "a"),
  (
    "Dangling closing brackets",
    `{ "hello": 1234 }}}`,
    Json.Object(list{("hello", Number("1234"))}),
    "}}",
  ),
  (
    "Dangling closing braces",
    `{ "hello": 1234 }]]`,
    Json.Object(list{("hello", Number("1234"))}),
    "]]",
  ),
  (
    "Dangling closing sign",
    `{ "hello": [1234] }-`,
    Json.Object(list{("hello", Array(list{Number("1234")}))}),
    "-",
  ),
]

partiallyOkInputs->Belt.Array.forEach(((name, input, expected, remaining)) => {
  test(`[JSON] ${name} partially succeeds`, t => {
    switch run(input) {
    | Ok(output, rest) if output == expected && remaining == rest =>
      t->pass(~message=`Should succeed with "${rest}" remaining`, ())
    | Ok(output, rest) =>
      t->fail(
        ~message=`Shouldn't succeed with "${output->Json.toString}" and "${rest}" remaining; expected "${expected->Json.toString}" and "${remaining}" remaining`,
        (),
      )
    | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
    }
  })
})

let erroneousInputs = [
  //
  "       +{}",
  "_{}",
  `|{"hello": 1}`,
  `a{}`,
  `~{f}`,
]

erroneousInputs->Belt.Array.forEachWithIndex((index, input) => {
  test(`[JSON] Object ${index->string_of_int} fails`, t => {
    switch run(input) {
    | Ok(_, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
    | Error(err) => t->pass(~message=`Should fail with "${err}"`, ())
    }
  })
})
