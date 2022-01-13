type t<'t> = Parser(string => result<('t, string), string>)

type run<'a> = (t<'a>, string) => result<('a, string), string>
type bind<'a, 'b> = (t<'a>, 'a => t<'b>) => t<'b>
type return<'a> = 'a => t<'a>
type map<'a, 'b> = (t<'a>, 'a => 'b) => t<'b>
type andThen<'a, 'b> = (t<'a>, t<'b>) => t<('a, 'b)>
type orElse<'a> = (t<'a>, t<'a>) => t<'a>
type satisfy = (char => bool) => t<char>
type char_ = char => t<char>
type choice<'a> = array<t<'a>> => t<'a>
type anyOf = array<char> => t<char>
type apply<'a, 'b> = (t<'a>, t<'a => 'b>) => t<'b>
type lift2<'a, 'b, 'c> = (t<'a>, ('a, 'b) => 'c, t<'b>) => t<'c>
type sequence<'a> = list<t<'a>> => t<list<'a>>
type zeroOrMore<'a> = (t<'a>, string) => (list<'a>, string)
type many<'a> = t<'a> => t<list<'a>>
type atLeastOne<'a> = t<'a> => t<list<'a>>
type keepLeft<'a, 'b> = (t<'a>, t<'b>) => t<'a>
type keepRight<'a, 'b> = (t<'a>, t<'b>) => t<'b>
type between<'a, 'b, 'c> = (t<'a>, t<'b>, t<'c>) => t<'a>
type separatedBy<'a, 'b> = (t<'a>, t<'b>) => t<list<'a>>
type separatedBy1<'a, 'b> = (t<'a>, t<'b>) => t<list<'a>>

type makeRecursive<'a> = (t<'a> => t<'a>) => t<'a>
type makeForwardRef<'a> = unit => (t<'a>, ref<t<'a>>)

let run: run<'a> = (t, input) => {
  let Parser(run) = t

  run(input)
}

let bind: bind<'a, 'b> = (t, fn) => Parser(
  input =>
    switch run(t, input) {
    | Error(msg) => Error(msg)
    | Ok((value, remaining)) => value->fn->run(remaining)
    },
)

let return: return<'a> = x => Parser(input => Ok((x, input)))
let map: map<'a, 'b> = (t, fn) => t->bind(x => x->fn->return)
let andThen: andThen<'a, 'b> = (p1, p2) => p1->bind(res1 => p2->bind(res2 => return((res1, res2))))

let satisfy: satisfy = predicate => Parser(
  input => {
    let char = try String.get(input, 0)->Some catch {
    | _ => None
    }

    switch char {
    | None => Error("No more input")
    | Some(char) if predicate(char) => Ok((char, String.sub(input, 1, String.length(input) - 1)))
    | Some(char) => Error("Unexpected " ++ Char.escaped(char))
    }
  },
)

let char: char_ = expected => satisfy(Char.equal(expected))
let orElse: orElse<'a> = (parser1, parser2) => Parser(
  input => {
    let result = run(parser1, input)

    switch result {
    | Ok(_) => result
    | Error(_) => run(parser2, input)
    }
  },
)

let choice: choice<'a> = parsers =>
  parsers->Belt.Array.reduce(Parser(_ => Error("Initial parser")), orElse)

let anyOf: anyOf = chars => chars->Belt.Array.map(char)->choice

let apply: apply<'a, 'b> = (parserA, parserB) =>
  parserB->bind(f => parserA->bind(x => return(f(x))))

let lift2: lift2<'a, 'b, 'c> = (parserA, fn, parserB) => apply(parserB, apply(parserA, return(fn)))

let rec sequence: sequence<'a> = (parsers: list<t<'a>>) =>
  switch parsers {
  | list{} => return(list{})
  | list{head, ...tail} => lift2(head, List.cons, sequence(tail))
  }

let rec zeroOrMore: zeroOrMore<'a> = (parser, input) =>
  switch run(parser, input) {
  | Error(_) => (list{}, input)
  | Ok((first, input)) =>
    let (values, input) = zeroOrMore(parser, input)

    (list{first, ...values}, input)
  }

let many: many<'a> = parser => Parser(input => Ok(zeroOrMore(parser, input)))

let atLeastOne: atLeastOne<'a> = parser =>
  parser->bind(head => many(parser)->bind(tail => return(list{head, ...tail})))

let keepLeft: keepLeft<'a, 'b> = (parserA, parserB) => parserA->andThen(parserB)->map(fst)

let keepRight: keepRight<'a, 'b> = (parserA, parserB) => parserA->andThen(parserB)->map(snd)

let between: between<'a, 'b, 'c> = (parserA, parserB, parserC) => {
  // Keeps parserA
  parserB->keepRight(parserA)->keepLeft(parserC)
}

let separatedBy1: separatedBy1<'a, 'b> = (parser, separator) => {
  let separators = keepRight(separator, parser)

  parser->andThen(many(separators))->map(((head, tail)) => list{head, ...tail})
}

let separatedBy: separatedBy<'a, 'b> = (parser, separator) => {
  parser->separatedBy1(separator)->orElse(return(list{}))
}

let string = x => {
  x
  ->Js.String2.split("")
  ->Belt.Array.map(String.get(_, 0))
  ->Belt.Array.map(char)
  ->Belt.List.fromArray
  ->sequence
  ->map(Belt.List.map(_, Char.escaped))
  ->map(Belt.List.toArray)
  ->map(Js.Array.joinWith(""))
}

let makeForwardRef: makeForwardRef<'t> = () => {
  let parser = Parser(_ => failwith("Not implemented"))
  let parserRef = ref(parser)

  (Parser(input => run(parserRef.contents, input)), parserRef)
}

let makeRecursive: makeRecursive<'t> = fn => {
  let (parser, parserRef) = makeForwardRef()

  parserRef := fn(parser)

  parserRef.contents
}

let optional = parser => {
  let some = parser->map(x => Some(x))
  let none = return(None)

  some->orElse(none)
}
