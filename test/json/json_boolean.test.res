module P = Parser

let true_ = Json_parser.true_

Ava.test("[JSON] valid true", t => {
  let result = P.run(true_, "true")

  switch result {
  | Ok((True, "")) => t->Ava.pass()
  | Ok((True, remaining)) =>
    t->Ava.fail(~message=`Parsing "true" should return an empty string but got "${remaining}"`, ())
  | Ok((ast, _)) =>
    t->Ava.fail(
      ~message=`Parsing "true" returns an incorrect AST of ${Json_parser.toString(ast)}`,
      (),
    )
  | Error(error) =>
    t->Ava.fail(~message=`Parsing "true" returns an error of message: "${error}"`, ())
  }
})

Ava.test("[JSON] invalid true", t => {
  let result = P.run(true_, "ttrue")

  switch result {
  | Ok((Null, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
  }

  let result = P.run(true_, "false")

  switch result {
  | Ok((True, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
  }

  let result = P.run(true_, "{}}")

  switch result {
  | Ok((True, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
  }
})

let false_ = Json_parser.false_

Ava.test("[JSON] valid false", t => {
  let result = P.run(false_, "false")

  switch result {
  | Ok((False, "")) => t->Ava.pass()
  | Ok((False, remaining)) =>
    t->Ava.fail(~message=`Parsing "false" should return an empty string but got "${remaining}"`, ())
  | Ok((ast, _)) =>
    t->Ava.fail(
      ~message=`Parsing "false" returns an incorrect AST of ${Json_parser.toString(ast)}`,
      (),
    )
  | Error(error) =>
    t->Ava.fail(~message=`Parsing "false" returns an error of message: "${error}"`, ())
  }
})

Ava.test("[JSON] invalid false", t => {
  let result = P.run(false_, "tfalse")

  switch result {
  | Ok((False, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "false_" should return an error message`, ())
  }

  let result = P.run(false_, " true")

  switch result {
  | Ok((False, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "false_" should return an error message`, ())
  }

  let result = P.run(false_, "{}}")

  switch result {
  | Ok((False, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "false_" should return an error message`, ())
  }
})
