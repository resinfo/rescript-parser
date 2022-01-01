open Ava

module P = Parser
module Helpers = Json_helpers

let run = P.run(Helpers.digit)

test("Digit succeeds", t => {
  Belt.Range.forEach(0, 9, index => {
    let asString = index->string_of_int
    let result = run(asString)

    switch result {
    | Ok((output, "")) => t->true_(output == asString, ())
    | Ok((_, remaining)) =>
      t->fail(~message=`Parsing digit had remaining characters: "${remaining}"`, ())
    | Error(error) => t->fail(~message=`Failure to parse digit: ${error}`, ())
    }
  })
})

test("Digit partially succeeds", t => {
  let result = run("10")

  switch result {
  | Ok("1", "0") => t->pass(~message="Should be a digit with a char remaining", ())
  | _ => t->fail()
  }

  let result = run("1hello")

  switch result {
  | Ok("1", "hello") => t->pass(~message="Should be a digit with a char remaining", ())
  | _ => t->fail()
  }
})

test("Digit fails", t => {
  let result = run("hello")

  switch result {
  | Ok(_) => t->fail(~message="Should fail outright", ())
  | Error(error) => t->pass(~message=`Fails to parse, reason: "${error}"`, ())
  }
})

let run = P.run(Helpers.digits)

test("Digits succeeds", t => {
  let result = run("1")

  switch result {
  | Ok("1", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Passed with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  let result = run("123")

  switch result {
  | Ok("123", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Passed with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  let result = run("123asdf")

  switch result {
  | Ok("123", "asdf") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Passed with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }
})

test("Digits fails", t => {
  let result = run(" 123")

  switch result {
  | Ok(_, _) => t->fail()
  | Error(_) => t->pass()
  }

  let result = run("not even close")

  switch result {
  | Ok(_, _) => t->fail()
  | Error(_) => t->pass()
  }
})

let run = P.run(Helpers.exponent)

test("Exponent succeeds", t => {
  switch run("e-1234") {
  | Ok("e-1234", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("E-1234") {
  | Ok("E-1234", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("e1234") {
  | Ok("e1234", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("E1234") {
  | Ok("E1234", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("e0") {
  | Ok("e0", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("E0") {
  | Ok("E0", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Exponent partially succeeds", t => {
  switch run("e-1234hello") {
  | Ok("e-1234", "hello") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("e-1234  1") {
  | Ok("e-1234", "  1") => t->pass()
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

let run = P.run(Helpers.fraction)

test("Fraction succeeds", t => {
  switch run(".1234") {
  | Ok(".1234", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run(".4") {
  | Ok(".4", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run(".0000345") {
  | Ok(".0000345", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Fraction partially succeeds", t => {
  switch run(".654hello") {
  | Ok(".654", "hello") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run(".1  1") {
  | Ok(".1", "  1") => t->pass()
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

let run = P.run(Helpers.sign)

test("Sign succeeds", t => {
  switch run("-") {
  | Ok('-', "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("+") {
  | Ok('+', "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }
})

test("Sign partially succeeds", t => {
  switch run("+1") {
  | Ok('+', "1") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-1  1") {
  | Ok('-', "1  1") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("+     sdf1  1") {
  | Ok('+', "     sdf1  1") => t->pass()
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

let run = P.run(Helpers.integer)

test("Integer succeeds", t => {
  switch run("1") {
  | Ok("1", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("0") {
  | Ok("0", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("1234") {
  | Ok("1234", "") => t->pass()
  | Ok(ok, rest) => t->fail(~message=`Should not have "${ok}" with "${rest}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("-1") {
  | Ok("-1", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-0") {
  | Ok("-0", "") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-10002345") {
  | Ok("-10002345", "") => t->pass()
  | Ok((res, remaining)) =>
    t->fail(~message=`Should not be okay of "${res}" with "${remaining}" remaining`, ())
  | Error(_) => t->fail(~message="Should not be an error", ())
  }
})

test("Integer partially succeeds", t => {
  switch run("1ert") {
  | Ok("1", "ert") => t->pass()
  | Ok(_) | Error(_) => t->fail()
  }

  switch run("-1  1") {
  | Ok("-1", "  1") => t->pass()
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
