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
  | DModule(module_)
  | DExport(exportName, literal)
and module_ = Module(moduleName, list<definition>, option<list<(exportName, literal)>>)

let charListToString = ls =>
  ls->Belt.List.map(Char.escaped)->Belt.List.reduce("", Js.String2.concat)

let whitespace = P.anyOf([' ', '\n', '\r', '\t'])
let manyWhitespace = P.many(whitespace)
let betweenManyWhitespace = P.between(_, manyWhitespace, manyWhitespace)

let doubleQuote = P.char('"')
let lParen = P.char('(')
let rParen = P.char(')')
let lBracket = P.char('[')
let rBracket = P.char(']')
let lBrace = P.char('{')
let rBrace = P.char('}')

let betweenBrackets = P.between(_, lBracket, rBracket)
let betweenParens = P.between(_, lParen, rParen)
let betweenBraces = P.between(_, lBrace, rBrace)
let betweenDoubleQuote = P.between(_, doubleQuote, doubleQuote)

let digit = P.satisfy(c => c >= '0' && '9' >= c)
let uppercase = P.satisfy(c => c >= 'a' && 'z' >= c)
let lowercase = P.satisfy(c => c >= 'A' && 'Z' >= c)
let stringLiteral = {
  // TODO: FIX UP
  P.choice([
    //
    uppercase,
    lowercase,
    digit,
  ])
  ->P.many
  ->betweenDoubleQuote
  ->P.map(charListToString)
}

let numberLiteral = {
  P.atLeastOne(digit)->P.map(charListToString)
}

let identifier = {
  P.atLeastOne(P.choice([uppercase, lowercase]))->P.map(charListToString)
}

let literal = P.makeRecursive(p => {
  let arrayLiteral = P.many(p)->betweenBrackets
  let recordLiteral = {
    open Parser
    let pair = char(':')->keepRight(identifier)->andThen(p)
    let pairs = atLeastOne(pair)->betweenManyWhitespace

    lParen->keepRight(pairs)->keepLeft(rParen)
  }

  let lambdaLiteral = {
    open Parser
    let keyword = string("lam")->betweenManyWhitespace
    let arg = identifier->betweenManyWhitespace
    let args = many(arg)->betweenBrackets->betweenManyWhitespace

    keyword->keepRight(args)->andThen(p)->betweenParens
  }

  let function = {
    open Parser
    let named = identifier->map(name => FNamed(name))
    let anon = lambdaLiteral->map(((args, body)) => FAnon(args, body))

    named->orElse(anon)->betweenManyWhitespace
  }

  let execution = {
    open Parser
    function->andThen(many(p))->betweenParens
  }

  let blocks = {
    let variable =
      P.string("let")
      ->betweenManyWhitespace
      ->P.keepRight(identifier->betweenManyWhitespace)
      ->P.andThen(p)
      ->betweenParens

    let fn = {
      open Parser
      let keyword = string("fun")->betweenManyWhitespace
      let name = identifier->betweenManyWhitespace
      let arg = identifier->betweenManyWhitespace
      let args = many(arg)->betweenBrackets->betweenManyWhitespace

      keyword->keepRight(name)->andThen(args)->andThen(p)->betweenParens
    }

    P.choice([
      variable->P.map(((name, body)) => BVariable(name, body)),
      fn->P.map((((name, args), body)) => BFunction(name, args, body)),
      p->P.map(literal => BLiteral(literal)),
    ])
    ->betweenManyWhitespace
    ->P.atLeastOne
    ->betweenBraces
  }

  P.choice([
    P.string("true")->P.map(_ => LTrue),
    P.string("false")->P.map(_ => LFalse),
    numberLiteral->P.map(n => LNumber(n)),
    stringLiteral->P.map(s => LString(s)),
    identifier->P.map(i => LIdentifier(i)),
    arrayLiteral->P.map(xs => LArray(xs)),
    recordLiteral->P.map(xs => LRecord(xs)),
    lambdaLiteral->P.map(((args, body)) => LLambda(args, body)),
    execution->P.map(((function, args)) => LExecution(function, args)),
    blocks->P.map(blocks => {
      let rev = Belt.List.reverse

      switch blocks->rev {
      | list{BLiteral(head), ...rest} => LBlock(rest->rev, Some(head))
      | _ => LBlock(blocks, None)
      }
    }),
  ])->betweenManyWhitespace
})

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

let parser = {
  let identifier = P.choice([uppercase, lowercase])->P.many->P.map(charListToString)

  // let literal = {
  //   P.makeRecursive(literal => {
  //     let number = P.atLeastOne(digit)->P.map(charListToString)->P.map(n => LNumber(n))
  //     let string = {
  //       let validChar = P.choice([digit, uppercase, lowercase])->P.many->P.map(charListToString)

  //       validChar->P.between(doubleQuote, doubleQuote)->P.map(s => LString(s))
  //     }

  //     let true_ = P.string("true")->P.map(_ => LTrue)
  //     let false_ = P.string("false")->P.map(_ => LFalse)
  //     let array = {
  //       P.many(literal)->P.between(P.char('['), P.char(']'))->P.map(xs => LArray(xs))
  //     }

  //     P.choice([string, number, true_, false_, array])->betweenManyWhitespace
  //   })
  // }

  let definition = {
    P.char('a')
  }
  and module_ = {
    let defs = {
      P.between(_, P.char('{'), P.char('}'))
    }

    P.char('(')
    ->P.keepRight(manyWhitespace)
    ->P.keepRight(P.string("module"))
    ->P.keepRight(manyWhitespace)
    ->P.keepRight(identifier)
    ->P.keepLeft(manyWhitespace)
    ->P.keepLeft(P.char(')'))
    ->P.map(xs => Module("", list{}, None))
  }
}
