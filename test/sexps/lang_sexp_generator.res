let rec listLast = l => {
  switch l {
  | list{} => None
  | list{a} => Some(a)
  | list{_, ...rest} => listLast(rest)
  }
}

type t = Lang_sexp_parser.t

let rec stringifyDef = (def: Lang_sexp_parser.definition, indent) => {
  let ws = Js.String.repeat(indent, "\t")
  let newline = "\n" ++ ws ++ ""

  switch def {
  | DVariable(identifier, literal) => `var ${identifier} = ${stringifyLiteral(literal, indent)}`
  | DFunction(identifier, args, literal) => {
      let args = args->Belt.List.toArray->Belt.Array.joinWith(",", x => x)

      `function ${identifier}(${args}) {` ++
      newline ++
      Js.String.repeat(indent + 1, "\t") ++
      `return ` ++
      stringifyLiteral(literal, indent + 1) ++
      `;` ++
      newline ++ `}`
    }
  | DModule(Module(name, definitions, exports)) => {
      let defs =
        definitions
        ->Belt.List.map(def => stringifyDef(def, indent + 1))
        ->Belt.List.toArray
        ->Belt.Array.joinWith("", x => newline ++ x)
      let exports =
        exports
        ->Belt.List.map(((k, v)) => `${k}: ${stringifyLiteral(v, indent)}`)
        ->Belt.List.toArray
        ->Belt.Array.joinWith("", x => x ++ ",")

      newline ++
      `var ${name} = (() => {` ++
      defs ++
      newline ++
      `return {` ++
      newline ++
      ws ++
      exports ++
      newline ++
      newline ++
      `}` ++
      newline ++ `})();`
    }
  | DExport(_exportName, _literal) => ""
  }
}
and stringifyLiteral = (literal: Lang_sexp_parser.literal, indent) => {
  let ws = Js.String.repeat(indent, "\t")
  let newline = "\n" ++ ws

  switch literal {
  | LIdentifier(identifier) => identifier
  | LNumber(number) => number
  | LString(string) => `"${string}"`
  | LTrue => "true"
  | LFalse => "false"
  | LArray(literals) => {
      let body = {
        literals
        ->Belt.List.map(x => stringifyLiteral(x, indent))
        ->Belt.List.toArray
        ->Belt.Array.joinWith(", ", x => x)
      }

      `[${body}]`
    }
  | LRecord(pairs) => {
      let x = 1

      ws ++ `{` ++ newline ++ `}`
    }
  | LLambda(params, literal) => {
      ()
      `function() {
      return
    }`
    }
  | LExecution(function, args) => {
      ()
      ``
    }
  | LBlock(bodies, return) => ""
  }
}

let generate = (t: t) => {
  switch t {
  | Module(name, defs, exports) =>
    let file =
      defs
      ->Belt.List.map(def => stringifyDef(def, 0))
      ->Belt.List.toArray
      ->Belt.Array.joinWith("\n\n", x => x)

    let exports = {
      let exports =
        exports
        ->Belt.List.toArray
        ->Belt.Array.joinWith(",\n", ((name, literal)) =>
          `\t${name}: ${stringifyLiteral(literal, 0)}`
        )

      `module.exports = {\n` ++ exports ++ `\n}`
    }

    file ++ "\n\n" ++ exports
  }
}
