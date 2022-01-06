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
  | Object(list<(string, t)>)
  | Number(string)
  | String(string)
  | Array(list<t>)

let rec toString = t => {
  switch t {
  | Null => "Null"
  | True => "True"
  | False => "False"
  | Number(amount) => `Number(${amount})`
  | String(string) => `String(${string})`
  | Object(_xs) => `Object()`
  | Array(xs) => {
      let rec stringify = xs => {
        switch xs {
        | list{} => ""
        | list{head, ...rest} => "\n" ++ toString(head) ++ stringify(rest)
        }
      }

      "[" ++ stringify(xs) ++ "]"
    }
  }
}

module P = Parser
module Helpers = Json_helpers

let _charToString = c => c->int_of_char->Js.String.fromCharCode

let null = _ => Null
let true_ = _ => True
let false_ = _ => False
let number = number => Number(number)
let string = string => String(string)
let array = xs => Array(xs)
let object = xs => Object(xs)

let json = P.makeRecursive(p => {
  let {manyWhitespace} = module(Helpers)

  let arrayElements = {
    let empty = manyWhitespace->P.between(P.char('['), P.char(']'))->P.map(_ => list{})

    let nonEmpty =
      P.many(p)
      ->P.separatedBy(P.char(','))
      ->P.between(P.char('['), P.char(']'))
      ->P.map(Belt.List.flatten(_))

    P.choice([empty, nonEmpty])
  }

  let objectPairs = {
    let nonEmpty = {
      let keyValuePair =
        Helpers.string
        ->P.between(manyWhitespace, manyWhitespace)
        ->P.keepLeft(P.char(':'))
        ->P.andThen(p)

      P.many(keyValuePair)
      ->P.separatedBy1(P.char(','))
      ->P.map(Belt.List.flatten(_))
      ->P.between(P.char('{'), P.char('}'))
    }

    let empty = {
      manyWhitespace->P.between(P.char('{'), P.char('}'))->P.map(_ => list{})
    }

    P.choice([empty, nonEmpty])
  }

  P.choice([
    objectPairs->P.map(object),
    arrayElements->P.map(array),
    Helpers.number->P.map(number),
    Helpers.string->P.map(string),
    P.string("null")->P.map(null),
    P.string("true")->P.map(true_),
    P.string("false")->P.map(false_),
  ])->P.between(manyWhitespace, manyWhitespace)
})
