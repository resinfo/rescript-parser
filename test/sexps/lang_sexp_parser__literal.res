module Ast = Lang_sexp_parser__ast
module P = Parser

let concatStringList = Belt.List.reduce(_, "", Js.String2.concat)

let charListToString = ls => ls->Belt.List.map(Char.escaped)->concatStringList

let whitespace = P.anyOf([' ', '\n', '\r', '\t'])
let manyWhitespace = P.many(whitespace)
let betweenManyWhitespace = P.between(_, manyWhitespace, manyWhitespace)

let lParen = P.char('(')
let rParen = P.char(')')
let lBracket = P.char('[')
let rBracket = P.char(']')
let lBrace = P.char('{')
let rBrace = P.char('}')

let betweenBrackets = P.between(_, lBracket, rBracket)
let betweenParens = P.between(_, lParen, rParen)
let betweenBraces = P.between(_, lBrace, rBrace)

let hyphen = P.char('-')
let underbar = P.char('_')

let uppercase = P.satisfy(c => c >= 'a' && 'z' >= c)
let lowercase = P.satisfy(c => c >= 'A' && 'Z' >= c)

let zero = P.char('0')
let oneThroughNine = P.satisfy(c => c >= '1' && '9' >= c)

let digit = zero->P.orElse(oneThroughNine)
let digits = P.atLeastOne(digit)->P.map(Belt.List.map(_, Char.escaped))->P.map(concatStringList)

let sign = P.anyOf(['+', '-'])

let fraction = P.char('.')->P.andThen(digits)->P.map(((dot, digits)) => Char.escaped(dot) ++ digits)

let exponent = {
  let toString = (((char, sign), digits)) => {
    char->Char.escaped ++
    sign->Belt.Option.map(Char.escaped)->Belt.Option.getWithDefault("") ++
    digits
  }

  P.choice([
    P.char('e')->P.andThen(P.optional(sign))->P.andThen(digits)->P.map(toString),
    P.char('E')->P.andThen(P.optional(sign))->P.andThen(digits)->P.map(toString),
  ])
}

let integer = {
  let toString = ((sign, digits)) => Char.escaped(sign) ++ digits
  let oneThroughNineThenDigits = {
    oneThroughNine->P.andThen(digits)->P.map(toString)
  }
  let signThenDigit = {
    P.char('-')->P.andThen(digit->P.map(Char.escaped))->P.map(toString)
  }
  let signThenOneThroughNineThenDigits = {
    P.char('-')
    ->P.andThen(oneThroughNine)
    ->P.andThen(digits)
    ->P.map((((sign, digit), rest)) => Char.escaped(sign) ++ Char.escaped(digit) ++ rest)
  }

  P.choice([
    //
    oneThroughNineThenDigits,
    digit->P.map(Char.escaped),
    signThenOneThroughNineThenDigits,
    signThenDigit,
  ])
}

let numberLiteral = {
  let fraction = P.optional(fraction)
  let exponent = P.optional(exponent)

  integer
  ->P.andThen(fraction)
  ->P.andThen(exponent)
  ->P.map((((integer, fraction), exponent)) =>
    integer ++ fraction->Belt.Option.getWithDefault("") ++ exponent->Belt.Option.getWithDefault("")
  )
}

let doubleQuote = P.char('"')

let unescapedChar = {
  P.satisfy(ch => ch !== '\\' && ch !== '"')->P.map(int_of_char)->P.map(Js.String.fromCharCode)
}

let escapedChar = {
  [
    ("\/", '/'),
    ("\\\"", '"'),
    ("\\", '\\'),
    ("\b", '\b'),
    // ("\f", 'f'), // Not implementing
    ("\n", '\n'),
    ("\r", '\r'),
    ("\t", '\t'),
  ]
  ->Belt.Array.map(((toMatch, result)) => {
    P.string(toMatch)->P.map(_ => result->int_of_char->Js.String.fromCharCode)
  })
  ->P.choice
}

@val
external hexToInt: (string, @as(16) _) => int = "parseInt"

let unicodeChar = {
  let backslash = P.char('\\')
  let uChar = P.char('u')
  let hexdigit = P.satisfy(c =>
    switch c {
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' => true
    | _ => false
    }
  )
  let fourHexDigits = {
    hexdigit->P.andThen(hexdigit)->P.andThen(hexdigit)->P.andThen(hexdigit)
  }

  backslash
  ->P.keepRight(uChar)
  ->P.keepRight(fourHexDigits)
  ->P.map(((((a, b), c), d)) => {
    [a, b, c, d]->Obj.magic->Js.String.fromCharCodeMany->hexToInt->Js.String.fromCharCode
  })
}

let quotedString = {
  let jsChar = unescapedChar->P.orElse(escapedChar)->P.orElse(unicodeChar)

  P.many(jsChar)->P.between(doubleQuote, doubleQuote)->P.map(concatStringList)
}

let stringLiteral = quotedString

let identifier = {
  let withoutLeadingHyphen =
    P.atLeastOne(P.choice([uppercase, lowercase, underbar]))
    ->P.andThen(P.many(P.choice([uppercase, lowercase, digit, hyphen, underbar])))
    ->P.map(((a, b)) => Belt.List.concat(a, b))

  withoutLeadingHyphen->P.map(charListToString)
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
    let named = identifier->map(name => Ast.FNamed(name))
    let anon = lambdaLiteral->map(((args, body)) => Ast.FAnon(args, body))

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
      variable->P.map(((name, body)) => Ast.BVariable(name, body)),
      fn->P.map((((name, args), body)) => Ast.BFunction(name, args, body)),
      p->P.map(literal => Ast.BLiteral(literal)),
    ])
    ->betweenManyWhitespace
    ->P.atLeastOne
    ->betweenBraces
  }

  P.choice([
    P.string("true")->P.map(_ => Ast.LTrue),
    P.string("false")->P.map(_ => Ast.LFalse),
    numberLiteral->P.map(n => Ast.LNumber(n)),
    stringLiteral->P.map(s => Ast.LString(s)),
    identifier->P.map(i => Ast.LIdentifier(i)),
    arrayLiteral->P.map(xs => Ast.LArray(xs)),
    recordLiteral->P.map(xs => Ast.LRecord(xs)),
    lambdaLiteral->P.map(((args, body)) => Ast.LLambda(args, body)),
    execution->P.map(((function, args)) => Ast.LExecution(function, args)),
    blocks->P.map(blocks => {
      let rev = Belt.List.reverse

      switch blocks->rev {
      | list{BLiteral(head), ...rest} => Ast.LBlock(rest->rev, Some(head))
      | _ => LBlock(blocks, None)
      }
    }),
  ])->betweenManyWhitespace
})
