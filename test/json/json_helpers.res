/*
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

module Option = Belt.Option
module P = Parser

let charToString = c => c->int_of_char->Js.String.fromCharCode

let rec charListToString = chars => {
  switch chars {
  | list{} => ""
  | list{char, ...rest} => char ++ charListToString(rest)
  }
}

let digit = P.satisfy(c => c >= '0' && '9' >= c)->P.map(charToString)

let digits = P.atLeastOne(digit)->P.map(charListToString)

let sign = P.anyOf(['+', '-'])

let integer = ""
let fraction = P.char('.')->P.andThen(digits)->P.map(((dot, digits)) => charToString(dot) ++ digits)

let exponent = {
  let toString = (((char, sign), digits)) => {
    char->charToString ++ sign->Option.map(charToString)->Option.getWithDefault("") ++ digits
  }

  P.choice([
    //
    P.char('e')->P.andThen(P.optional(sign))->P.andThen(digits)->P.map(toString),
    P.char('E')->P.andThen(P.optional(sign))->P.andThen(digits)->P.map(toString),
  ])
}
