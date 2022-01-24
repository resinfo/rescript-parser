open Ava

module P = Res_parser

let run = P.run(Json.digit)

let remaining = P.State.remaining
let shouldNotPass = ((_, s)) => `Should not pass with "${remaining(s)}" remaining`

let shouldNotFail = "Should not fail"

test("Digit succeeds", t => {
  Belt.Range.forEach(0, 9, index => {
    let asString = index->string_of_int

    switch run(asString) {
    | Ok((output, state)) if P.State.remaining(state) == "" => t->true_(output == asString, ())
    | Ok((_, {input: remaining})) =>
      t->fail(~message=`Parsing digit had remaining characters: "${remaining}"`, ())
    | Error(error) => t->fail(~message=`Failure to parse digit: "${error.message}"`, ())
    }
  })
})

Test_runners.runTests(
  //
  ~parser=Json.digit,
  ~makeName=(input, _) => `[JSON Digit] Run with "${input}"`,
  ~specs=[("10", "1", "0"), ("1hello", "1", "hello")],
)

Test_runners.runFailureTests(
  //
  ~parser=Json.digit,
  ~makeName=input => `[JSON Digit] Fail with "${input}"`,
  ~specs=["hello"],
)

Test_runners.runTests(
  ~parser=Json.digits,
  ~makeName=(input, _) => `[JSON digit] Run with "${input}"`,
  ~specs=[
    //
    ("1", "1", ""),
    ("123", "123", ""),
    ("123asdf", "123", "asdf"),
  ],
)

test("Digits fails", t => {
  switch run(" 123") {
  | Ok(_, _) => t->fail()
  | Error(_) => t->pass()
  }

  switch run("not even close") {
  | Ok(_, _) => t->fail()
  | Error(_) => t->pass()
  }
})

let run = P.run(Json.exponent)

test("Exponent succeeds", t => {
  switch run("e-1234") {
  | Ok("e-1234", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("E-1234") {
  | Ok("E-1234", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("e1234") {
  | Ok("e1234", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("E1234") {
  | Ok("E1234", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("e0") {
  | Ok("e0", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("E0") {
  | Ok("E0", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Exponent partially succeeds", t => {
  switch run("e-1234hello") {
  | Ok("e-1234", state) if remaining(state) == "hello" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("e-1234  1") {
  | Ok("e-1234", state) if remaining(state) == "  1" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Exponent fails", t => {
  switch run("  E0") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("e--1") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("e-") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("ye ") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }
})

let run = P.run(Json.fraction)

test("Fraction succeeds", t => {
  switch run(".1234") {
  | Ok(".1234", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run(".4") {
  | Ok(".4", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run(".0000345") {
  | Ok(".0000345", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Fraction partially succeeds", t => {
  switch run(".654hello") {
  | Ok(".654", state) if remaining(state) == "hello" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run(".1  1") {
  | Ok(".1", state) if remaining(state) == "  1" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Fraction fails", t => {
  switch run("  .1") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run(".e1") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run(". .1") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("..") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("..11") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }
})

let run = P.run(Json.sign)

test("Sign succeeds", t => {
  switch run("-") {
  | Ok('-', state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("+") {
  | Ok('+', state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Sign partially succeeds", t => {
  switch run("+1") {
  | Ok('+', s) if remaining(s) == "1" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-1  1") {
  | Ok('-', s) if remaining(s) == "1  1" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("+     sdf1  1") {
  | Ok('+', s) if remaining(s) == "     sdf1  1" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Sign fails", t => {
  switch run("  +") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("  1-") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }
})

let run = P.run(Json.integer)

test("Integer succeeds", t => {
  switch run("1") {
  | Ok("1", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("0") {
  | Ok("0", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("1234") {
  | Ok("1234", state) if remaining(state) == "" => t->pass()
  | Ok(ok, s) =>
    t->fail(~message=`Should not have "${ok}" with "${s->P.State.remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("-1") {
  | Ok("-1", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-0") {
  | Ok("-0", state) if remaining(state) == "" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-10002345") {
  | Ok("-10002345", state) if remaining(state) == "" => t->pass()
  | Ok((_, s)) => t->fail(~message=`Should not have ok with "${remaining(s)}" remaining`, ())
  | Error(_) => t->fail(~message="Should not be an error", ())
  }
})

test("Integer partially succeeds", t => {
  switch run("1ert") {
  | Ok("1", s) if remaining(s) == "ert" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-1  1") {
  | Ok("-1", s) if remaining(s) == "  1" => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Integer fails", t => {
  switch run("  +") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("  1") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }

  switch run("  h1-") {
  | Error(_) => t->pass()
  | Ok(_) => t->fail()
  }
})

let run = P.run(Json.unescapedChar)

test("Unescaped char succeeds", t => {
  switch run("a") {
  | Ok("a", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("b") {
  | Ok("b", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("c") {
  | Ok("c", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("d") {
  | Ok("d", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("9") {
  | Ok("9", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("-") {
  | Ok("-", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }
  switch run(" ") {
  | Ok(" ", state) if remaining(state) == "" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }
})

test("Unescaped char partially succeeds", t => {
  switch run(`a"`) {
  | Ok("a", s) if remaining(s) == `"` => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("a\\") {
  | Ok("a", s) if remaining(s) == "\\" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("a\\\"") {
  | Ok("a", s) if remaining(s) == "\\\"" => t->pass()
  | Ok(_) => t->fail()
  | Error(_) => t->fail(~message="Should not fail", ())
  }
})

test("Unescaped char fails", t => {
  switch run(`"`) {
  | Ok(_) => t->fail(~message="Should not parse escaped char", ())
  | Error(_) => t->pass()
  }

  switch run("\\") {
  | Ok(_) => t->fail(~message="Should not parse escaped char", ())
  | Error(_) => t->pass()
  }
})

let run = P.run(Json.escapedChar)

let successes = [
  ("\\\"", "\""),
  ("\\\\", "\\"),
  ("\\/", "/"),
  ("\\b", "\b"),
  ("\\n", "\n"),
  ("\\r", "\r"),
  ("\\t", "\t"),
]

successes->Belt.Array.forEach(((input, expected)) => {
  test(`[Escaped char] "${input}" succeeds`, t => {
    switch run(input) {
    | Ok(j, s) =>
      t->Ava.true_(
        j == expected,
        ~message=`Should not succeed with "${j}" and "${remaining(s)}" remaining`,
        (),
      )
    | Error(err) => t->fail(~message=`Should not fail with "${err.message}"`, ())
    }
  })
})

let partialSuccesses = [
  ("\\\"  ", `"`, "  "),
  ("\\\\hello", "\\", "hello"),
  ("\/\/", "/", "\/"),
  ("\\b-1234gfd", "\b", "-1234gfd"),
  ("\\n l ll ll \n", "\n", " l ll ll \n"),
  ("\\r\\n", "\r", "\\n"),
  ("\\t\lll", "\t", "\lll"),
]

partialSuccesses->Belt.Array.forEach(((input, expected, remaining_)) => {
  test(`[Escaped char] "${input}" partially succeeds`, t => {
    switch run(input) {
    | Ok(e, r) if e == expected && remaining(r) == remaining_ => t->pass()
    | Ok(e, r) =>
      t->fail(~message=`Shouldn't succeed with "${e}" and "${remaining(r)}" remaining`, ())
    | Error(err) => t->fail(~message=`Shouldn't fail with "${err.message}"`, ())
    }
  })
})

test("Escaped char fails", t => {
  switch run(" \"") {
  | Ok(_) => t->fail()
  | Error(_) => t->pass()
  }

  switch run(" \\\"") {
  | Ok(_) => t->fail()
  | Error(_) => t->pass()
  }

  switch run("asfds\t") {
  | Ok(_) => t->fail()
  | Error(_) => t->pass()
  }
})

let run = P.run(Json.unicodeChar)

test("Unicode char succeeds", t => {
  switch run("\u0041") {
  | Ok("A", state) if remaining(state) == "" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\u0031") {
  | Ok("1", state) if remaining(state) == "" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\u0028") {
  | Ok("(", state) if remaining(state) == "" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\u0101") {
  | Ok(x, _) if x == Js.String.fromCharCode(257) /* "ā" */ => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }
})

test("Unicode char partially succeeds", t => {
  switch run("\u0041asdf") {
  | Ok("A", s) if remaining(s) == "asdf" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\\u0041asdf") {
  | Ok("A", s) if remaining(s) == "asdf" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\u0031999") {
  | Ok("1", s) if remaining(s) == "999" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\u0028   lol") {
  | Ok("(", s) if remaining(s) == "   lol" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }

  switch run("\u0101\u0101") {
  | Ok(x, s) if x == Js.String.fromCharCode(257) /* "ā" */ && remaining(s) == "\u0101" => t->pass()
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->fail(~message=shouldNotFail, ())
  }
})

test("Unicode char fails", t => {
  switch run("  \u0041asdf") {
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->pass(~message=shouldNotFail, ())
  }

  switch run("\\\u0031999") {
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->pass(~message=shouldNotFail, ())
  }

  switch run("l\u0028   lol") {
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->pass(~message=shouldNotFail, ())
  }

  switch run("/\u0101\u0101") {
  | Ok(x) => t->fail(~message=shouldNotPass(x), ())
  | Error(_) => t->pass(~message=shouldNotFail, ())
  }
})
