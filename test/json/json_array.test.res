open Ava

module P = Res_parser

let run = P.run(Json.parse)

let shouldNotPass = ((_, remaining)) => `Should not pass with "${remaining}" remaining`

let shouldNotFail = "Should not fail"

skip("[JSON] Array succeeds", _t => {
  ()
})

// test("[JSON] Array succeeds", t => {
//   switch run("[]") {
//   | Ok(Array(list{}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`["1"]`) {
//   | Ok(Array(list{String("1")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[1]`) {
//   | Ok(Array(list{Number("1")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[1, 2]`) {
//   | Ok(Array(list{Number("1"), Number("2")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[1,2]`) {
//   | Ok(Array(list{Number("1"), Number("2")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[1, "2"]`) {
//   | Ok(Array(list{Number("1"), String("2")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[null, "1"]`) {
//   | Ok(Array(list{Null, String("1")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[false, true, 14.2]`) {
//   | Ok(Array(list{False, True, Number("14.2")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[      false,     true, 14.2]`) {
//   | Ok(Array(list{False, True, Number("14.2")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[      false,     true,

//   14.2]`) {
//   | Ok(Array(list{False, True, Number("14.2")}), "") => t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }

//   switch run(`[1, [1, true, [3, null]]]`) {
//   | Ok(
//       Array(list{Number("1"), Array(list{Number("1"), True, Array(list{Number("3"), Null})})}),
//       "",
//     ) =>
//     t->pass()
//   | Ok(x) => t->fail(~message=shouldNotPass(x), ())
//   | Error(_) => t->fail(~message=shouldNotFail, ())
//   }
// })
