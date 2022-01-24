open Ava

module P = Res_parser

skip("[JSON] valid true", _t => {
  ()
})

// Ava.test("[JSON] valid true", t => {
//   let result = P.run(Json.parse, "true")

//   switch result {
//   | Ok((True, "")) => t->Ava.pass()
//   | Ok((True, remaining)) =>
//     t->Ava.fail(~message=`Parsing "true" should return an empty string but got "${remaining}"`, ())
//   | Ok((ast, _)) =>
//     t->Ava.fail(~message=`Parsing "true" returns an incorrect AST of ${Json.toString(ast)}`, ())
//   | Error(error) =>
//     t->Ava.fail(~message=`Parsing "true" returns an error of message: "${error}"`, ())
//   }
// })

// Ava.test("[JSON] invalid true", t => {
//   let result = P.run(Json.parse, "ttrue")

//   switch result {
//   | Ok((Null, _remaining)) => t->Ava.fail()
//   | Ok((_ast, _)) => t->Ava.fail()
//   | Error(_) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
//   }

//   let result = P.run(Json.parse, "false")

//   switch result {
//   | Ok((True, _remaining)) => t->Ava.fail()
//   | Ok((_ast, _)) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
//   | Error(_) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
//   }

//   let result = P.run(Json.parse, "-true")

//   switch result {
//   | Ok((True, _remaining)) => t->Ava.fail()
//   | Ok((_ast, _)) => t->Ava.fail()
//   | Error(_) => t->Ava.pass(~message=`Parsing "true_" should return an error message`, ())
//   }
// })

// Ava.test("[JSON] valid false", t => {
//   let result = P.run(Json.parse, "false")

//   switch result {
//   | Ok((False, "")) => t->Ava.pass()
//   | Ok((False, remaining)) =>
//     t->Ava.fail(~message=`Parsing "false" should return an empty string but got "${remaining}"`, ())
//   | Ok((ast, _)) =>
//     t->Ava.fail(~message=`Parsing "false" returns an incorrect AST of ${Json.toString(ast)}`, ())
//   | Error(error) =>
//     t->Ava.fail(~message=`Parsing "false" returns an error of message: "${error}"`, ())
//   }
// })

// Ava.test("[JSON] invalid false", t => {
//   let result = P.run(Json.parse, "tfalse")

//   switch result {
//   | Ok((False, _remaining)) => t->Ava.fail()
//   | Ok((_ast, _)) => t->Ava.fail()
//   | Error(_) => t->Ava.pass(~message=`Parsing "false_" should return an error message`, ())
//   }

//   let result = P.run(Json.parse, "{false")

//   switch result {
//   | Ok((False, _remaining)) => t->Ava.fail()
//   | Ok((_ast, _)) => t->Ava.fail()
//   | Error(_) => t->Ava.pass(~message=`Parsing "false_" should return an error message`, ())
//   }
// })
