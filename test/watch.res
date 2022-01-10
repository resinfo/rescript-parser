let file = Node.Fs.readFileAsUtf8Sync("test.clj")

let input = `(module test {${file}})`
let output = Parser.run(Lang_sexp_parser.parser, input)

switch output {
| Error(err) => err
| Ok(ast, _) => Lang_sexp_generator.generate(ast)
}->Node.Fs.writeFileAsUtf8Sync("test.clj.js", _)
