/*
 **  Full spec available here:
 **  https://www.json.org/json-en.html
 **
 **
 ** json
 **   element
 **
 ** value
 **   object
 **   array
 **   string
 **   number
 **   "true"
 **   "false"
 **   "null"
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
 ** string
 **   '"' characters '"'
 **
 ** characters
 **   ""
 **   character characters
 **
 ** character
 **   '0020' . '10FFFF' - '"' - '\'
 **   '\' escape
 **
 ** escape
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
 ** hex
 **   digit
 **   'A' . 'F'
 **   'a' . 'f'
 **
 ** number
 **   integer fraction exponent
 **
 ** integer
 **   digit
 **   onenine digits
 **   '-' digit
 **   '-' onenine digits
 **
 ** digits
 **   digit
 **   digit digits
 **
 ** digit
 **   '0'
 **   onenine
 **
 ** onenine
 **   '1' . '9'
 **
 ** fraction
 **   ""
 **   '.' digits
 **
 ** exponent
 **   ""
 **   'E' sign digits
 **   'e' sign digits
 **
 ** sign
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

type rec t =
  | Null
  | True
  | False
  | Number(string)

let toString = t => {
  switch t {
  | Null => "Null"
  | True => "True"
  | False => "False"
  | Number(amount) => `Number(${amount})`
  }
}

module P = Parser

let _charToString = c => c->int_of_char->Js.String.fromCharCode

let null = P.string("null")->P.map(_ => Null)
let true_ = P.string("true")->P.map(_ => True)
let false_ = P.string("false")->P.map(_ => False)

let number = {
  let digit = P.satisfy(c => c >= '0' && '9' >= c)->P.map(_charToString)

  let digits = {
    P.choice([
      digit,
      digit
      ->P.andThen(P.many(digit))
      ->P.map(((head, rest)) => Belt.List.reduce(rest, head, Js.String2.concat)),
    ])
  }

  let sign = P.anyOf(['+', '-'])->P.optional

  let integer = ""
  let fraction = ""
  let exponent = P.char('E')->P.andThen(sign)

  ""
}
