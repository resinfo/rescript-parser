open Ava

module P = Parser

let number = Json.number
let run = P.run(Json.json)

test("[JSON] Number succeeds", t => {
  switch run("1234") {
  | Ok(Number("1234"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("-1234") {
  | Ok(Number("-1234"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("-0") {
  | Ok(Number("-0"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("0") {
  | Ok(Number("0"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("1234.1234") {
  | Ok(Number("1234.1234"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("1234.1e1") {
  | Ok(Number("1234.1e1"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("0.12E-6543") {
  | Ok(Number("0.12E-6543"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }
})

test("[JSON] Number partially succeeds", t => {
  switch run("0.12E-6543hello") {
  | Ok(Number("0.12E-6543"), "hello") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("111hello") {
  | Ok(Number("111"), "hello") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("-533.1hello") {
  | Ok(Number("-533.1"), "hello") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("-1234.1..1hello") {
  | Ok(Number("-1234.1"), "..1hello") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed otherwise", ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }
})

test("[JSON] Number fails", t => {
  switch run("_-1") {
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(_) => t->pass()
  }

  switch run("--0.12E-6543") {
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(_) => t->pass()
  }

  switch run("a0.12") {
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(_) => t->pass()
  }
})
