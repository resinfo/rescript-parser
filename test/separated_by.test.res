open Ava

module Helpers = Json_helpers
module P = Parser

test("Separated by simple", t => {
  let parser =
    P.char('a')
    ->P.separatedBy1(P.char(','))
    ->P.map(Belt.List.map(_, Helpers.charToString))
    ->P.map(Helpers.concatStringList)

  let run = P.run(parser)

  switch run("a,a") {
  | Ok("aa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a,,a") {
  | Ok("a", ",,a") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a,a,a,a") {
  | Ok("aaaa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a") {
  | Ok("a", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a,b,c") {
  | Ok("a", ",b,c") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  let manyWhitespace = P.many(P.char(' '))
  let commaWithWhitespace = P.char(',')->P.between(manyWhitespace, manyWhitespace)
  let parser = P.char('a')->P.separatedBy1(commaWithWhitespace)

  let run = P.run(parser)

  switch run("a, a,a,   a    ,a") {
  | Ok(list{'a', 'a', 'a', 'a', 'a'}, "") => t->pass()
  | Ok(_, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
  | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
  }

  switch run("a") {
  | Ok(list{'a'}, "") => t->pass()
  | Ok(_, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
  | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
  }

  switch run("") {
  | Ok(_, rest) => t->fail(~message=`Shouldn't succeed with "${rest}" remaining`, ())
  | Error(err) => t->pass(~message=`Shouldn't fail with "${err}"`, ())
  }
})

test("Separated by many simple", t => {
  let parser =
    P.char('a')
    ->P.separatedBy(P.char(','))
    ->P.map(Belt.List.map(_, Helpers.charToString))
    ->P.map(Helpers.concatStringList)

  let run = P.run(parser)

  switch run("a,a") {
  | Ok("aa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a,a,a,a") {
  | Ok("aaaa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a") {
  | Ok("a", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a,b,c") {
  | Ok("a", ",b,c") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }
})

test("Separated by at least one", t => {
  let atLeastOneComma = P.atLeastOne(P.char(','))
  let parser =
    P.char('a')
    ->P.separatedBy(atLeastOneComma)
    ->P.map(Belt.List.map(_, Helpers.charToString))
    ->P.map(Helpers.concatStringList)

  let run = P.run(parser)

  switch run("a,,,a") {
  | Ok("aa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  switch run("a,,,,,a,,a,a,,,a") {
  | Ok("aaaaa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail()
  }

  let commaSeparatedByWhitespace = {
    Helpers.manyWhitespace->P.andThen(P.char(','))->P.andThen(Helpers.manyWhitespace)
  }
  let parser =
    P.char('a')
    ->P.separatedBy(commaSeparatedByWhitespace)
    ->P.map(Belt.List.map(_, Helpers.charToString))
    ->P.map(Helpers.concatStringList)

  let run = P.run(parser)

  switch run("a, a") {
  | Ok("aa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("a      ,a") {
  | Ok("aa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }

  switch run("a      ,a,a,a      ,a") {
  | Ok("aaaaa", "") => t->pass()
  | Ok(_, remaining) => t->fail(~message=`Should not pass with "${remaining}" remaining`, ())
  | Error(_) => t->fail(~message="Should not fail", ())
  }
})
