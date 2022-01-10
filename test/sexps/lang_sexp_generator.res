let rec listLast = l => {
  switch l {
  | list{} => None
  | list{a} => Some(a)
  | list{_, ...rest} => listLast(rest)
  }
}

type t = Lang_sexp_parser.t

let stringifyIdentifier = Js.String.replaceByRe(%re(`/-/g`), "__HYPHEN__")
let tab = x => Js.String2.repeat("", x * 2)

let joinWith = Belt.Array.joinWith

let rec stringifyDef = (def: Lang_sexp_parser.definition, indent) => {
  let newline = "\n"

  switch def {
  | DVariable(identifier, literal) =>
    `var ${identifier->stringifyIdentifier} = ${stringifyLiteral(literal, indent + 1)}`
  | DFunction(identifier, args, literal) => stringifyFunction(identifier, args, literal, indent + 1)
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
  let newline = "\n"

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
  | LRecord(pairs) =>
    [
      `{`,
      pairs
      ->Belt.List.toArray
      ->joinWith(",\n", ((name, l)) => `"${name}": ${stringifyLiteral(l, indent + 1)}`),
      `}`,
    ]->Js.Array2.joinWith("\n")
  | LLambda(params, literal) => {
      let params = params->Belt.List.toArray->Js.Array2.joinWith(", ")

      `function(${params}) {` ++
      newline ++
      tab(indent) ++
      `return ` ++
      stringifyLiteral(literal, indent) ++
      newline ++ `}`
    }
  | LExecution(function, args) =>
    switch function {
    | FNamed(identifier) =>
      stringifyIdentifier(identifier) ++
      "(" ++
      args->Belt.List.toArray->Belt.Array.joinWith(", ", stringifyLiteral(_, indent)) ++ ")"
    | FAnon(params, literal) => {
        let body = stringifyFunction("", params, literal, indent + 1)

        `(${body})();`
      }
    }
  | LBlock(bodies, return) => {
      let bodies =
        bodies
        ->Belt.List.toArray
        ->Belt.Array.map(x => {
          switch x {
          | BVariable(identifier, literal) =>
            `var ${identifier} = ${stringifyLiteral(literal, indent + 1)};`
          | BFunction(identifier, args, literal) =>
            `(${stringifyFunction(identifier, args, literal, indent + 1)})();`
          | BLiteral(literal) => stringifyLiteral(literal, indent + 1)
          }
        })
        ->Belt.Array.joinWith("\n", x => x)
      let return =
        return
        ->Belt.Option.map(x => `return ${stringifyLiteral(x, indent + 1)}`)
        ->Belt.Option.getWithDefault("")

      `(function() {${bodies} ${return}; })()`
    }
  }
}
and stringifyFunction = (identifier, args, literal, indent: int) => {
  let ws = tab(indent)
  let newline = "\n" ++ ws ++ ""
  let args = args->Belt.List.toArray->Belt.Array.joinWith(",", x => x)

  `function ${stringifyIdentifier(identifier)}(${args}) {` ++
  newline ++
  tab(indent + 1) ++
  `return ` ++
  stringifyLiteral(literal, indent + 1) ++
  `;` ++
  newline ++ `}`
}
and stringifyBlock = (block: Lang_sexp_parser.block, indent) => {
  switch block {
  | BVariable(identifier, literal) =>
    `var ${identifier} = ${stringifyLiteral(literal, indent + 1)};`
  | BFunction(identifier, args, literal) =>
    `(${stringifyFunction(identifier, args, literal, indent + 1)})();`
  | BLiteral(literal) => stringifyLiteral(literal, indent + 1)
  }
}
and setBlockName = (name, block: Lang_sexp_parser.block) => {
  switch block {
  | BVariable(identifier, literal) => Lang_sexp_parser.BVariable(name ++ identifier, literal)
  | BFunction(identifier, args, literal) => BFunction(name ++ identifier, args, literal)
  | BLiteral(literal) => BLiteral(literal)
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
          tab(1) ++ `${name}: ${stringifyLiteral(literal, 0)}`
        )

      `module.exports = {\n` ++ exports ++ `\n}`
    }

    (name, file ++ "\n\n" ++ exports)
  }
}
