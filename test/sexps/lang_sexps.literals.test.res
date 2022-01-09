module P = Parser
module Sexp = Lang_sexp_parser
open Ava

let run = P.run(Sexp.literal)
let successes = [
  //
  ("true", "true", Sexp.LTrue),
  ("true w/ whitespace", "    true  ", LTrue),
  ("false", "false", LFalse),
  ("false w/ whitespace", "    false  ", LFalse),
  ("simple number", "10", LNumber("10")),
  ("simple number w/ whitespace", "    10 ", LNumber("10")),
  ("simple++ number", "0019652200", LNumber("0019652200")),
  ("simple++ number w/ whitespace", "    0019652200 ", LNumber("0019652200")),
  ("simple identifier", "hello", LIdentifier("hello")),
  (
    "simple identifier w/ whitespace",
    `
  
  hello
  `,
    LIdentifier("hello"),
  ),
  ("simple array", `[1]`, LArray(list{LNumber("1")})),
  (
    "simple array w/ whitespace",
    ` [    1
  ]  `,
    LArray(list{LNumber("1")}),
  ),
  (
    "simple array++",
    `[1 "2" [false]]`,
    LArray(list{
      LNumber("1"),
      LString("2"),
      LArray(list{
        //
        LFalse,
      }),
    }),
  ),
  (
    "simple array++ w/ whitespace",
    `   [     1
    "2" [
            false]]`,
    LArray(list{
      LNumber("1"),
      LString("2"),
      LArray(list{
        //
        LFalse,
      }),
    }),
  ),
  ("simple record", `(:foo 1)`, LRecord(list{("foo", LNumber("1"))})),
  (
    "simple record with spaces",
    `     ( :foo
  
  
    1 )
  `,
    LRecord(list{("foo", LNumber("1"))}),
  ),
  (
    "simple record++",
    `(:foo 1 :bar "baz" :quux false)`,
    LRecord(list{
      //
      ("foo", LNumber("1")),
      ("bar", LString("baz")),
      ("quux", LFalse),
    }),
  ),
  (
    "simple record++ w/ whitespace",
    `
    (
      :foo  1 :bar
      
      "baz"
:quux          false ) `,
    LRecord(list{
      //
      ("foo", LNumber("1")),
      ("bar", LString("baz")),
      ("quux", LFalse),
    }),
  ),
  ("simple lambda (no args)", `(lam [] 1)`, LLambda(list{}, LNumber("1"))),
  ("simple lambda (one arg)", `(lam [a] 1)`, LLambda(list{"a"}, LNumber("1"))),
  ("simple lambda (multiple args)", `(lam [a b c] 1)`, LLambda(list{"a", "b", "c"}, LNumber("1"))),
  ("simple lambda++ (no args)", `(lam [] a)`, LLambda(list{}, LIdentifier("a"))),
  ("simple lambda++ (one arg)", `(lam [a] a)`, LLambda(list{"a"}, LIdentifier("a"))),
  (
    "simple lambda++ (multiple args)",
    `(lam [a b c] [a b c])`,
    LLambda(
      list{"a", "b", "c"},
      LArray(list{LIdentifier("a"), LIdentifier("b"), LIdentifier("c")}),
    ),
  ),
  (
    "lambda with lambda body",
    `(lam [a] (lam [b] 12))`,
    LLambda(list{"a"}, LLambda(list{"b"}, LNumber("12"))),
  ),
  (
    "simple named function execution empty",
    `(add 1)`,
    LExecution(FNamed("add"), list{LNumber("1")}),
  ),
  ("simple named function execution one arg", `(add)`, LExecution(FNamed("add"), list{})),
  (
    "simple named function execution one arg w/whitespace",
    `  (   add
   1   )  `,
    LExecution(FNamed("add"), list{LNumber("1")}),
  ),
  (
    "simple anonymous function execution",
    `((lam [x] x) 1)`,
    LExecution(
      //
      FAnon(list{"x"}, LIdentifier("x")),
      list{LNumber("1")},
    ),
  ),
  (
    "nested anonymous function execution",
    `((lam [a] ((lam [b] "hello") a)) 1)`,
    LExecution(
      FAnon(
        list{"a"},
        LExecution(
          FAnon(list{"b"}, LString("hello")),
          //
          list{LIdentifier("a")},
        ),
      ),
      list{LNumber("1")},
    ),
  ),
  (
    "nested anonymous function execution w/ whitespace",
    ` ( (   lam
    [a]
    ( (       lam[b]     "hello")a)
    )1)`,
    LExecution(
      FAnon(
        list{"a"},
        LExecution(
          FAnon(list{"b"}, LString("hello")),
          //
          list{LIdentifier("a")},
        ),
      ),
      list{LNumber("1")},
    ),
  ),
  ("simple block", `{ "hello" }`, LBlock(list{}, Some(LString("hello")))),
  (
    "simple block string w/ whitespace",
    `{
      "hello"
    }`,
    LBlock(list{}, Some(LString("hello"))),
  ),
  (
    "simple block execution w/ whitespace",
    `{
      (log "hello")
    }`,
    LBlock(list{}, Some(LExecution(FNamed("log"), list{LString("hello")}))),
  ),
  (
    "simple block no return",
    `{
      (let hello 1)
    }`,
    LBlock(list{BVariable("hello", LNumber("1"))}, None),
  ),
  (
    "simple block execution no return",
    `{
      (let hello (log 1))
    }`,
    LBlock(
      list{
        //
        BVariable("hello", LExecution(FNamed("log"), list{LNumber("1")})),
      },
      None,
    ),
  ),
  (
    "multiline block execution no return",
    `{
      (let hello "hello")
      (let world (concat hello "world"))
    }`,
    LBlock(
      list{
        //
        BVariable("hello", LString("hello")),
        BVariable(
          "world",
          LExecution(
            FNamed("concat"),
            list{
              //
              LIdentifier("hello"),
              LString("world"),
            },
          ),
        ),
      },
      None,
    ),
  ),
  (
    "multiline block execution w/ return",
    `{
      (let hello "hello")
      (let world (concat hello "world"))

      world
    }`,
    LBlock(
      list{
        //
        BVariable("hello", LString("hello")),
        BVariable(
          "world",
          LExecution(
            FNamed("concat"),
            list{
              //
              LIdentifier("hello"),
              LString("world"),
            },
          ),
        ),
      },
      Some(LIdentifier("world")),
    ),
  ),
  (
    "simple block w/ return",
    `{
      (let hello 1)

      hello
    }`,
    LBlock(
      list{
        //
        BVariable("hello", LNumber("1")),
      },
      Some(LIdentifier("hello")),
    ),
  ),
  (
    "multiline block w/o return",
    `{
      (let hello 1)
      (let world 2)
    }`,
    LBlock(
      list{
        //
        BVariable("hello", LNumber("1")),
        BVariable("world", LNumber("2")),
      },
      None,
    ),
  ),
  (
    "multiline block w/ whitespace",
    `{
      (let world "world")
      (let bar (concat "hello" world))

      bar
    }`,
    LBlock(
      list{
        BVariable("world", LString("world")),
        BVariable(
          "bar",
          LExecution(
            FNamed("concat"),
            //
            list{LString("hello"), LIdentifier("world")},
          ),
        ),
      },
      Some(LIdentifier("bar")),
    ),
  ),
  (
    "multiline return lambda",
    `
  {
    (let x 1)
    
    (lam [a] (add a x))
  }
  `,
    LBlock(
      list{BVariable("x", LNumber("1"))},
      Some(
        LLambda(
          list{"a"},
          LExecution(
            FNamed("add"),
            list{
              //
              LIdentifier("a"),
              LIdentifier("x"),
            },
          ),
        ),
      ),
    ),
  ),
  (
    "simple function block w/ return",
    `
  {
    (fun hello [name] "hello")

    hello
  }`,
    LBlock(
      list{
        BFunction(
          "hello",
          list{
            //
            "name",
          },
          LString("hello"),
        ),
      },
      Some(LIdentifier("hello")),
    ),
  ),
  ("simple nested block", `{ { 1 } }`, LBlock(list{}, Some(LBlock(list{}, Some(LNumber("1")))))),
]

successes->Belt.Array.forEach(((name, input, expected)) => {
  test(`[Sexp lang] "${name}" success`, t => {
    switch run(input) {
    | Ok(output, "") if output == expected => t->pass(~message=``, ())
    | Ok(_ast, rest) =>
      t->fail(
        ~message=`Shouldn't succeed with ${_ast->Sexp.literalToString} and "${rest}" remaining`,
        (),
      )
    | Error(err) => t->fail(~message=`Shouldn't fail with "${err}"`, ())
    }
  })
})
