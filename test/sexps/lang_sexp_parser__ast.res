module P = Parser

type identifier = string
type functionArg = string

type rec literal =
  | LIdentifier(identifier)
  | LNumber(string)
  | LString(string)
  | LTrue
  | LFalse
  | LArray(list<literal>)
  | LRecord(list<(string, literal)>)
  | LLambda(list<functionArg>, literal)
  | LExecution(function, list<literal>)
  | LBlock(list<block>, option<literal>)
and function =
  | FNamed(string)
  | FAnon(list<functionArg>, literal)
and block =
  | BVariable(identifier, literal)
  | BFunction(identifier, list<functionArg>, literal)
  | BLiteral(literal)

type moduleName = string
type exportName = string

type rec definition =
  | DVariable(identifier, literal)
  | DFunction(identifier, list<functionArg>, literal)
  | DModule(t)
  | DExport(exportName, literal)
and t = Module(moduleName, list<definition>, list<(exportName, literal)>)

let rec literalToString = literal => {
  let concat = Js.String2.concat
  let indent = Js.String2.concat("\n\t")
  module L = Belt.List
  module A = Belt.Array

  let rec loop = literal => {
    switch literal {
    | LIdentifier(identifier) => `LIdentifier(${identifier})`
    | LNumber(number) => `LNumber(${number})`
    | LString(string) => `LString("${string}")`
    | LTrue => `LTrue`
    | LFalse => `LFalse`
    | LArray(literals) => {
        let nodes = {
          literals->L.map(loop)->L.toArray->Belt.Array.joinWith("", indent)
        }

        `LArray(${nodes})`
      }
    | LRecord(pairs) => {
        let nodes = {
          pairs->L.map(((k, v)) => k ++ `: ` ++ loop(v))->L.toArray->A.joinWith("", indent)
        }

        `LRecord(${nodes})`
      }
    | LLambda(args, body) => {
        let args = args->L.toArray->A.joinWith("", concat("\n\t\t"))
        let body = body->loop->concat("\n\t\t")

        `LLambda([${args}], ${body})`
      }

    | LExecution(fn, args) => {
        let fn = fn->functionToString->concat("\n\t\t")
        let args = args->L.map(loop)->L.toArray->A.joinWith("", concat("\n\t\t"))

        `LExecution(${fn}, [${args}])`
      }

    | LBlock(bodies, return) => {
        let return = switch return {
        | None => `None`
        | Some(literal) => `Some(${literal->loop})`
        }

        let body = bodies->Belt.List.map(blockToString)->Belt.List.toArray->Js.Array2.joinWith(", ")

        `LBlock(list{${body}}, ${return})`
      }
    }
  }

  loop(literal)
}
and functionToString = fn => {
  switch fn {
  | FNamed(name) => `FNamed(${name})`
  | FAnon(args, body) => {
      let concat = Js.String2.concat("\n\t\t")
      let args = args->Belt.List.toArray->Belt.Array.joinWith("", concat)
      let body = body->literalToString->concat

      `FAnon([${args}], ${body})`
    }
  }
}
and blockToString = block => {
  switch block {
  | BVariable(identifier, literal) => `BVariable(${identifier}, ${literal->literalToString})`
  | BFunction(name, args, body) => {
      let args = args->Belt.List.toArray->Belt.Array.joinWith("", Js.String2.concat(", "))

      `BFunction(${name}, ${args}, ${body->literalToString})`
    }
  | BLiteral(literal) => `BLiteral(${literal->literalToString})`
  }
}

and definitionToString = definition => {
  switch definition {
  | DVariable(identifier, _literal) => `DVariable(${identifier})`
  | DFunction(identifier, functionArgs, _literal) =>
    `DFunction(${identifier}, ${functionArgs
      ->Belt.List.toArray
      ->Belt.Array.joinWith(", ", x => x)})`
  | DModule(Module(name, _defs, _exports)) => `DModule(${name})`
  | DExport(exportName, literal) => `DExport(${exportName}, ${literalToString(literal)})`
  }
}
