/*
 ** json
 **   element
 **
 ** value
 **   object
 **   array
 **   string  ✔︎
 **   number  ✔︎
 **   "true"  ✔︎
 **   "false" ✔︎
 **   "null"  ✔︎
 **
 ** object
 **   '{' ws '}'
 **   '{' members '}'
 **
 ** members
 **   member
 **   member ',' members
 **
 ** member
 **   ws string ws ':' element
 **
 ** array
 **   '[' ws ']'
 **   '[' elements ']'
 **
 ** elements
 **   element
 **   element ',' elements
 **
 ** element
 **   ws value ws
 **
 ** string ✔︎
 **   '"' characters '"'
 **
 ** characters ✔︎
 **   ""
 **   character characters
 **
 ** character ✔︎
 **   '0020' . '10FFFF' - '"' - '\'
 **   '\' escape
 **
 ** escape ✔︎
 **   '"'
 **   '\'
 **   '/'
 **   'b'
 **   'f'
 **   'n'
 **   'r'
 **   't'
 **   'u' hex hex hex hex
 **
 ** hex ✔︎
 **   digit
 **   'A' . 'F'
 **   'a' . 'f'
 **
 ** number ✔︎
 **   integer fraction exponent
 **
 ** integer ✔︎
 **   digit
 **   onenine digits
 **   '-' digit
 **   '-' onenine digits
 **
 ** digits ✔︎
 **   digit
 **   digit digits
 **
 ** digit ✔︎
 **   '0'
 **   onenine
 **
 ** onenine ✔︎
 **   '1' . '9'
 **
 ** fraction ✔︎
 **   ""
 **   '.' digits
 **
 ** exponent ✔︎
 **   ""
 **   'E' sign digits
 **   'e' sign digits
 **
 ** sign ✔︎
 **   ""
 **   '+'
 **   '-'
 **
 ** ws
 **   ""
 **   '0020' ws
 **   '000A' ws
 **   '000D' ws
 **   '0009' ws
 */

module Option = Belt.Option
module P = Parser

@val
external hexToInt: (string, @as(16) _) => int = "parseInt"

let charToString = c => c->int_of_char->Js.String.fromCharCode

let rec concatStringList = chars => {
  switch chars {
  | list{} => ""
  | list{head, ...rest} => head ++ concatStringList(rest)
  }
}

let rec stringifyCharList = chars => {
  switch chars {
  | list{} => ""
  | list{head, ...rest} => charToString(head) ++ stringifyCharList(rest)
  }
}

let zero = P.char('0')
let oneThroughNine = P.satisfy(c => c >= '1' && '9' >= c)

let digit = zero->P.orElse(oneThroughNine)->P.map(charToString)
let digits = P.atLeastOne(digit)->P.map(concatStringList)

let sign = P.anyOf(['+', '-'])

let fraction = P.char('.')->P.andThen(digits)->P.map(((dot, digits)) => charToString(dot) ++ digits)

let exponent = {
  let toString = (((char, sign), digits)) => {
    char->charToString ++ sign->Option.map(charToString)->Option.getWithDefault("") ++ digits
  }

  P.choice([
    P.char('e')->P.andThen(P.optional(sign))->P.andThen(digits)->P.map(toString),
    P.char('E')->P.andThen(P.optional(sign))->P.andThen(digits)->P.map(toString),
  ])
}

let integer = {
  let toString = ((sign, digits)) => charToString(sign) ++ digits
  let oneThroughNineThenDigits = {
    oneThroughNine->P.andThen(digits)->P.map(toString)
  }
  let signThenDigit = {
    P.char('-')->P.andThen(digit)->P.map(toString)
  }
  let signThenOneThroughNineThenDigits = {
    P.char('-')
    ->P.andThen(oneThroughNine)
    ->P.andThen(digits)
    ->P.map((((sign, digit), rest)) => charToString(sign) ++ charToString(digit) ++ rest)
  }

  P.choice([
    //
    oneThroughNineThenDigits,
    digit,
    signThenOneThroughNineThenDigits,
    signThenDigit,
  ])
}

let number = {
  let fraction = P.optional(fraction)
  let exponent = P.optional(exponent)

  integer
  ->P.andThen(fraction)
  ->P.andThen(exponent)
  ->P.map((((integer, fraction), exponent)) =>
    integer ++ fraction->Option.getWithDefault("") ++ exponent->Option.getWithDefault("")
  )
}

let doubleQuote = P.char('"')

let unescapedChar = {
  P.satisfy(ch => ch !== '\\' && ch !== '"')->P.map(int_of_char)->P.map(Js.String.fromCharCode)
}

let escapedChar = {
  [
    ("\\\"", '"'), // quote
    ("\\\\", '\\'), // reverse solidus
    ("\\/", '/'), // solidus
    ("\\b", '\b'), // backspace
    // ("\f", 'f'), // formfeed (Not implementing)
    ("\\n", '\n'), // newline
    ("\\r", '\r'), // cr
    ("\\t", '\t'), // tab
  ]
  ->Belt.Array.map(((toMatch, result)) => {
    P.string(toMatch)->P.map(_ => result->int_of_char->Js.String.fromCharCode)
  })
  ->P.choice
}

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

let string = quotedString

let whitespace = P.anyOf([' ', '\n', '\t', '\r'])
let manyWhitespace = P.many(whitespace)
/*

/// Parse an unescaped char
let jUnescapedChar =
    satisfy (fun ch -> ch isnot '\\' && ch isnot '\"') "char"

/// Parse an escaped char
let jEscapedChar =
    [
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ]
    // convert each pair into a parser
    |> List.map (fun (toMatch,result) ->
        pstring toMatch >>% result)
    // and combine them into one
    |> choice
    <?> "escaped char" // set label

/// Parse a unicode char
let jUnicodeChar =

    // set up the "primitive" parsers
    let backslash = pchar '\\'
    let uChar = pchar 'u'
    let hexdigit = 
        anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])
    let fourHexDigits =
        hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit

    // convert the parser output (nested tuples)
    // to a char
    let convertToChar (((h1,h2),h3),h4) =
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char

    // set up the main parser
    backslash  >>. uChar >>. fourHexDigits
    |>> convertToChar


/// Parse a quoted string
let quotedString =
    let quote = pchar '\"' <?> "quote"
    let jchar = jUnescapedChar orElse jEscapedChar orElse jUnicodeChar

    // set up the main parser
    quote >>. manyChars jchar .>> quote

*/
