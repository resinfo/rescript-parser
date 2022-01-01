module P = Parser

let null = Json.null

Ava.test("[JSON] valid null", t => {
  let result = P.run(null, "null")

  switch result {
  | Ok((Null, "")) => t->Ava.pass()
  | Ok((Null, remaining)) =>
    t->Ava.fail(~message=`Parsing "null" should return an empty string but got "${remaining}"`, ())
  | Ok((ast, _)) =>
    t->Ava.fail(~message=`Parsing "null" returns an incorrect AST of ${Json.toString(ast)}`, ())
  | Error(error) =>
    t->Ava.fail(~message=`Parsing "null" returns an error of message: "${error}"`, ())
  }
})

Ava.test("[JSON] invalid null", t => {
  let result = P.run(null, "tnull")

  switch result {
  | Ok((Null, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "null" should return an error message`, ())
  }

  let result = P.run(null, "  ")

  switch result {
  | Ok((Json.Null, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "null" should return an error message`, ())
  }

  let result = P.run(null, "no")

  switch result {
  | Ok((Json.Null, _remaining)) => t->Ava.fail()
  | Ok((_ast, _)) => t->Ava.fail()
  | Error(_) => t->Ava.pass(~message=`Parsing "null" should return an error message`, ())
  }
})
