open Ava

module P = Parser

let run = P.run(Json.json)

let {map, forEach} = module(Belt.Array)

let readFiles = dirname => {
  Node.Fs.readdirSync("test/json/inputs/" ++ dirname)
  ->map(name => (name, "test/json/inputs/" ++ dirname ++ "/" ++ name))
  ->map(((filename, pathname)) => (filename, Node.Fs.readFileAsUtf8Sync(pathname)))
}

let passes = readFiles("passes")
let failures = readFiles("failures")
let partials = readFiles("partials")

passes->forEach(((name, file)) => {
  // TODO: One or two errors here
  skip(`[JSON] File "${name}" success`, t => {
    switch run(file) {
    | Ok(_, "") => t->pass(~message=`Should succeed`, ())
    | Ok(_, rest) => t->fail(~message=`Shouldn't partially succeed with "${rest}" remaining`, ())
    | Error(msg) => t->fail(~message=`Shouldn't fail with "${msg}"`, ())
    }
  })
})

partials->forEach(((name, file)) => {
  test(`[JSON] File "${name}" partial success`, t => {
    switch run(file) {
    | Ok(_, "") => t->fail(~message=`Shouldn't succeed`, ())
    | Ok(_, rest) => t->pass(~message=`Should partially succeed with "${rest}" remaining`, ())
    | Error(msg) => t->fail(~message=`Shouldn't fail with "${msg}"`, ())
    }
  })
})

failures->forEach(((name, file)) => {
  // TODO: Lots of errors here
  skip(`[JSON] File "${name}" failure`, t => {
    switch run(file) {
    | Ok(ast, rest) =>
      t->fail(~message=`Shouldn't succeed with "${ast->Json.toString}" and "${rest}" remaining`, ())
    | Error(msg) => t->pass(~message=`Should fail with "${msg}"`, ())
    }
  })
})
