open Ava

module P = Parser

let run = P.run(Json.parse)

test("[JSON] String succeeds", t => {
  switch run(`"1"`) {
  | Ok(String("1"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
  }

  switch run(`"hello"`) {
  | Ok(String("hello"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
  }

  switch run(`"   "`) {
  | Ok(String("   "), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
  }

  switch run(`""`) {
  | Ok(String(""), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
  }

  switch run(`"\u0050"`) {
  | Ok(String("P"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
  }

  switch run(`"\u0050\u0069\u0061n\u006F"`) {
  | Ok(String("Piano"), "") => t->pass()
  | Ok(_, _) => t->fail(~message="Should not succeed", ())
  | Error(msg) => t->fail(~message=`Should not fail with "${msg}"`, ())
  }
})
