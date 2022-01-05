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
  | String(string)
  | Array(list<t>)

let rec toString = t => {
  switch t {
  | Null => "Null"
  | True => "True"
  | False => "False"
  | Number(amount) => `Number(${amount})`
  | String(string) => `String(${string})`
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

let null = P.string("null")->P.map(_ => Null)
let true_ = P.string("true")->P.map(_ => True)
let false_ = P.string("false")->P.map(_ => False)

let number = Helpers.number->P.map(number => Number(number))
let string = Helpers.string->P.map(string => String(string))
let array = xs => Array(xs)

let json = P.makeRecursive(p => {
  let {manyWhitespace} = module(Helpers)

  let leftBracket = P.char('[')
  let rightBracket = P.char(']')

  let arr =
    P.many(p)
    ->P.separatedBy(P.char(','))
    ->P.between(leftBracket, rightBracket)
    ->P.map(Belt.List.flatten(_))
    ->P.map(array)
  // let elements = p->P.separatedBy1(commaWrappedInWhitespace)

  // let array = {
  //   let leftBracketWithWhitespace = leftBracket->P.keepRight(manyWhitespace)
  //   let rightBracketWithWhitespace = rightBracket->P.keepLeft(manyWhitespace)

  //   let element = p->P.between(manyWhitespace, manyWhitespace)
  //   let elements = P.makeRecursive(p => {
  //     P.choice([
  //       //
  //       element,
  //       // element
  //       // ->P.andThen(P.char(','))
  //       // ->P.andThen(p)
  //       P.many(element)->P.separatedBy1(commaWrappedInWhitespace)->P.map(xs => Array(list{})),
  //     ])
  //   })

  //   P.choice([
  //     //
  //     elements->P.between(leftBracket, rightBracket)->P.map(xs => Array(list{xs})),
  //     manyWhitespace->P.between(leftBracket, rightBracket)->P.map(_ => Array(list{})),
  //   ])
  // }

  P.choice([arr, number, string, null, true_, false_])->P.between(manyWhitespace, manyWhitespace)
})
