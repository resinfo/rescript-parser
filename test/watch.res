type formatOptions = {parser: [#babel]}

@module("prettier") @val
external format: (string, formatOptions) => string = "format"

@scope("console") @val
external clear: unit => unit = "clear"

let time = name => {
  Js.Console.timeStart(name)

  () => Js.Console.timeEnd(name)
}

clear()
Js.log("File changed")

let timeRead = time("[Read file]")

let file = Node.Fs.readFileAsUtf8Sync("test.clj")
timeRead()

let timeGenerate = time("[Generate]")

let input = `(module test {${file}})`
let output = Parser.run(Lang_sexp_parser.parser, input)

timeGenerate()

let (name, contents) = switch output {
| Error(err) => ("test", err)
| Ok(ast, _) => {
    let (name, contents) = Lang_sexp_generator.generate(ast)
    let timeFormat = time("[Format]")
    let contents = contents->format({parser: #babel})
    timeFormat()

    (name, contents)
  }
}

let timeWrite = time("[Time write]")
Node.Fs.writeFileAsUtf8Sync(name ++ ".clj.js", contents)

timeWrite()
