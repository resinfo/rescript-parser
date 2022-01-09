// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Char = require("rescript/lib/js/char.js");
var Parser = require("../../src/parser.cjs");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

function charListToString(ls) {
  return Belt_List.reduce(Belt_List.map(ls, Char.escaped), "", (function (prim0, prim1) {
                return prim0.concat(prim1);
              }));
}

var whitespace = Parser.anyOf([
      /* ' ' */32,
      /* '\n' */10,
      /* '\r' */13,
      /* '\t' */9
    ]);

var manyWhitespace = Parser.many(whitespace);

function betweenManyWhitespace(__x) {
  return Parser.between(__x, manyWhitespace, manyWhitespace);
}

var doubleQuote = Parser.$$char(/* '"' */34);

var lParen = Parser.$$char(/* '(' */40);

var rParen = Parser.$$char(/* ')' */41);

var lBracket = Parser.$$char(/* '[' */91);

var rBracket = Parser.$$char(/* ']' */93);

var lBrace = Parser.$$char(/* '{' */123);

var rBrace = Parser.$$char(/* '}' */125);

function betweenBrackets(__x) {
  return Parser.between(__x, lBracket, rBracket);
}

function betweenParens(__x) {
  return Parser.between(__x, lParen, rParen);
}

function betweenBraces(__x) {
  return Parser.between(__x, lBrace, rBrace);
}

function betweenDoubleQuote(__x) {
  return Parser.between(__x, doubleQuote, doubleQuote);
}

var digit = Parser.satisfy(function (c) {
      if (c >= /* '0' */48) {
        return /* '9' */57 >= c;
      } else {
        return false;
      }
    });

var uppercase = Parser.satisfy(function (c) {
      if (c >= /* 'a' */97) {
        return /* 'z' */122 >= c;
      } else {
        return false;
      }
    });

var lowercase = Parser.satisfy(function (c) {
      if (c >= /* 'A' */65) {
        return /* 'Z' */90 >= c;
      } else {
        return false;
      }
    });

var stringLiteral = Parser.map(Parser.between(Parser.many(Parser.choice([
                  uppercase,
                  lowercase,
                  digit
                ])), doubleQuote, doubleQuote), charListToString);

var numberLiteral = Parser.map(Parser.atLeastOne(digit), charListToString);

var identifier = Parser.map(Parser.atLeastOne(Parser.choice([
              uppercase,
              lowercase
            ])), charListToString);

var literal = Parser.makeRecursive(function (p) {
      var arrayLiteral = Parser.between(Parser.many(p), lBracket, rBracket);
      var pair = Parser.andThen(Parser.keepRight(Parser.$$char(/* ':' */58), identifier), p);
      var pairs = Parser.between(Parser.atLeastOne(pair), manyWhitespace, manyWhitespace);
      var recordLiteral = Parser.keepLeft(Parser.keepRight(lParen, pairs), rParen);
      var keyword = Parser.between(Parser.string("lam"), manyWhitespace, manyWhitespace);
      var arg = Parser.between(identifier, manyWhitespace, manyWhitespace);
      var args = Parser.between(Parser.between(Parser.many(arg), lBracket, rBracket), manyWhitespace, manyWhitespace);
      var lambdaLiteral = Parser.between(Parser.andThen(Parser.keepRight(keyword, args), p), lParen, rParen);
      var named = Parser.map(identifier, (function (name) {
              return {
                      TAG: /* FNamed */0,
                      _0: name
                    };
            }));
      var anon = Parser.map(lambdaLiteral, (function (param) {
              return {
                      TAG: /* FAnon */1,
                      _0: param[0],
                      _1: param[1]
                    };
            }));
      var $$function = Parser.between(Parser.orElse(named, anon), manyWhitespace, manyWhitespace);
      var execution = Parser.between(Parser.andThen($$function, Parser.many(p)), lParen, rParen);
      var variable = Parser.between(Parser.andThen(Parser.keepRight(Parser.between(Parser.string("let"), manyWhitespace, manyWhitespace), Parser.between(identifier, manyWhitespace, manyWhitespace)), p), lParen, rParen);
      var keyword$1 = Parser.between(Parser.string("fun"), manyWhitespace, manyWhitespace);
      var name = Parser.between(identifier, manyWhitespace, manyWhitespace);
      var arg$1 = Parser.between(identifier, manyWhitespace, manyWhitespace);
      var args$1 = Parser.between(Parser.between(Parser.many(arg$1), lBracket, rBracket), manyWhitespace, manyWhitespace);
      var fn = Parser.between(Parser.andThen(Parser.andThen(Parser.keepRight(keyword$1, name), args$1), p), lParen, rParen);
      var blocks = Parser.between(Parser.atLeastOne(Parser.between(Parser.choice([
                        Parser.map(variable, (function (param) {
                                return {
                                        TAG: /* BVariable */0,
                                        _0: param[0],
                                        _1: param[1]
                                      };
                              })),
                        Parser.map(fn, (function (param) {
                                var match = param[0];
                                return {
                                        TAG: /* BFunction */1,
                                        _0: match[0],
                                        _1: match[1],
                                        _2: param[1]
                                      };
                              })),
                        Parser.map(p, (function (literal) {
                                return {
                                        TAG: /* BLiteral */2,
                                        _0: literal
                                      };
                              }))
                      ]), manyWhitespace, manyWhitespace)), lBrace, rBrace);
      return Parser.between(Parser.choice([
                      Parser.map(Parser.string("true"), (function (param) {
                              return /* LTrue */0;
                            })),
                      Parser.map(Parser.string("false"), (function (param) {
                              return /* LFalse */1;
                            })),
                      Parser.map(numberLiteral, (function (n) {
                              return {
                                      TAG: /* LNumber */1,
                                      _0: n
                                    };
                            })),
                      Parser.map(stringLiteral, (function (s) {
                              return {
                                      TAG: /* LString */2,
                                      _0: s
                                    };
                            })),
                      Parser.map(identifier, (function (i) {
                              return {
                                      TAG: /* LIdentifier */0,
                                      _0: i
                                    };
                            })),
                      Parser.map(arrayLiteral, (function (xs) {
                              return {
                                      TAG: /* LArray */3,
                                      _0: xs
                                    };
                            })),
                      Parser.map(recordLiteral, (function (xs) {
                              return {
                                      TAG: /* LRecord */4,
                                      _0: xs
                                    };
                            })),
                      Parser.map(lambdaLiteral, (function (param) {
                              return {
                                      TAG: /* LLambda */5,
                                      _0: param[0],
                                      _1: param[1]
                                    };
                            })),
                      Parser.map(execution, (function (param) {
                              return {
                                      TAG: /* LExecution */6,
                                      _0: param[0],
                                      _1: param[1]
                                    };
                            })),
                      Parser.map(blocks, (function (blocks) {
                              var match = Belt_List.reverse(blocks);
                              if (!match) {
                                return {
                                        TAG: /* LBlock */7,
                                        _0: blocks,
                                        _1: undefined
                                      };
                              }
                              var head = match.hd;
                              switch (head.TAG | 0) {
                                case /* BVariable */0 :
                                case /* BFunction */1 :
                                    return {
                                            TAG: /* LBlock */7,
                                            _0: blocks,
                                            _1: undefined
                                          };
                                case /* BLiteral */2 :
                                    return {
                                            TAG: /* LBlock */7,
                                            _0: Belt_List.reverse(match.tl),
                                            _1: head._0
                                          };
                                
                              }
                            }))
                    ]), manyWhitespace, manyWhitespace);
    });

var definition = Parser.makeRecursive(function (def) {
      var export_ = Parser.between(Parser.andThen(Parser.keepRight(Parser.between(Parser.string("export"), manyWhitespace, manyWhitespace), Parser.between(identifier, manyWhitespace, manyWhitespace)), literal), lParen, rParen);
      var variable = Parser.between(Parser.andThen(Parser.keepRight(Parser.between(Parser.string("let"), manyWhitespace, manyWhitespace), Parser.between(identifier, manyWhitespace, manyWhitespace)), literal), lParen, rParen);
      var keyword = Parser.between(Parser.string("fun"), manyWhitespace, manyWhitespace);
      var name = Parser.between(identifier, manyWhitespace, manyWhitespace);
      var arg = Parser.between(identifier, manyWhitespace, manyWhitespace);
      var args = Parser.between(Parser.between(Parser.many(arg), lBracket, rBracket), manyWhitespace, manyWhitespace);
      var fn = Parser.between(Parser.andThen(Parser.andThen(Parser.keepRight(keyword, name), args), literal), lParen, rParen);
      var module_ = Parser.between(Parser.andThen(Parser.keepRight(Parser.between(Parser.string("module"), manyWhitespace, manyWhitespace), Parser.between(identifier, manyWhitespace, manyWhitespace)), Parser.between(Parser.many(def), lBrace, rBrace)), lParen, rParen);
      var moduleDef = Parser.map(module_, (function (param) {
              var match = Belt_List.partition(param[1], (function (x) {
                      if (x.TAG === /* DExport */3) {
                        return true;
                      } else {
                        return false;
                      }
                    }));
              var $$exports = Belt_List.keepMap(match[0], (function (x) {
                      if (x.TAG === /* DExport */3) {
                        return [
                                x._0,
                                x._1
                              ];
                      }
                      
                    }));
              return /* Module */{
                      _0: param[0],
                      _1: match[1],
                      _2: $$exports
                    };
            }));
      return Parser.between(Parser.choice([
                      Parser.map(variable, (function (param) {
                              return {
                                      TAG: /* DVariable */0,
                                      _0: param[0],
                                      _1: param[1]
                                    };
                            })),
                      Parser.map(fn, (function (param) {
                              var match = param[0];
                              return {
                                      TAG: /* DFunction */1,
                                      _0: match[0],
                                      _1: match[1],
                                      _2: param[1]
                                    };
                            })),
                      Parser.map(moduleDef, (function (m) {
                              return {
                                      TAG: /* DModule */2,
                                      _0: m
                                    };
                            })),
                      Parser.map(export_, (function (param) {
                              return {
                                      TAG: /* DExport */3,
                                      _0: param[0],
                                      _1: param[1]
                                    };
                            }))
                    ]), manyWhitespace, manyWhitespace);
    });

function literalToString(literal) {
  var indent = function (param) {
    return "\n\t".concat(param);
  };
  var loop = function (literal) {
    if (typeof literal === "number") {
      if (literal === /* LTrue */0) {
        return "LTrue";
      } else {
        return "LFalse";
      }
    }
    switch (literal.TAG | 0) {
      case /* LIdentifier */0 :
          return "LIdentifier(" + literal._0 + ")";
      case /* LNumber */1 :
          return "LNumber(" + literal._0 + ")";
      case /* LString */2 :
          return "LString(\"" + literal._0 + "\")";
      case /* LArray */3 :
          var nodes = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(literal._0, loop)), "", indent);
          return "LArray(" + nodes + ")";
      case /* LRecord */4 :
          var nodes$1 = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(literal._0, (function (param) {
                          return param[0] + ": " + loop(param[1]);
                        }))), "", indent);
          return "LRecord(" + nodes$1 + ")";
      case /* LLambda */5 :
          var args = Belt_Array.joinWith(Belt_List.toArray(literal._0), "", (function (param) {
                  return "\n\t\t".concat(param);
                }));
          var body = loop(literal._1).concat("\n\t\t");
          return "LLambda([" + args + "], " + body + ")";
      case /* LExecution */6 :
          var fn = functionToString(literal._0).concat("\n\t\t");
          var args$1 = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(literal._1, loop)), "", (function (param) {
                  return "\n\t\t".concat(param);
                }));
          return "LExecution(" + fn + ", [" + args$1 + "])";
      case /* LBlock */7 :
          var $$return = literal._1;
          var $$return$1 = $$return !== undefined ? "Some(" + loop($$return) + ")" : "None";
          var body$1 = Belt_List.toArray(Belt_List.map(literal._0, blockToString)).join(", ");
          return "LBlock(list{" + body$1 + "}, " + $$return$1 + ")";
      
    }
  };
  return loop(literal);
}

function functionToString(fn) {
  if (fn.TAG === /* FNamed */0) {
    return "FNamed(" + fn._0 + ")";
  }
  var concat = function (param) {
    return "\n\t\t".concat(param);
  };
  var args = Belt_Array.joinWith(Belt_List.toArray(fn._0), "", concat);
  var body = "\n\t\t".concat(literalToString(fn._1));
  return "FAnon([" + args + "], " + body + ")";
}

function blockToString(block) {
  switch (block.TAG | 0) {
    case /* BVariable */0 :
        return "BVariable(" + block._0 + ", " + literalToString(block._1) + ")";
    case /* BFunction */1 :
        var args = Belt_Array.joinWith(Belt_List.toArray(block._1), "", (function (param) {
                return ", ".concat(param);
              }));
        return "BFunction(" + block._0 + ", " + args + ", " + literalToString(block._2) + ")";
    case /* BLiteral */2 :
        return "BLiteral(" + literalToString(block._0) + ")";
    
  }
}

var parser = Parser.map(Parser.between(Parser.andThen(Parser.keepRight(Parser.between(Parser.string("module"), manyWhitespace, manyWhitespace), Parser.between(identifier, manyWhitespace, manyWhitespace)), Parser.between(Parser.many(definition), lBrace, rBrace)), lParen, rParen), (function (param) {
        var match = Belt_List.partition(param[1], (function (x) {
                if (x.TAG === /* DExport */3) {
                  return true;
                } else {
                  return false;
                }
              }));
        var $$exports = Belt_List.keepMap(match[0], (function (x) {
                if (x.TAG === /* DExport */3) {
                  return [
                          x._0,
                          x._1
                        ];
                }
                
              }));
        return /* Module */{
                _0: param[0],
                _1: match[1],
                _2: $$exports
              };
      }));

var P;

exports.P = P;
exports.charListToString = charListToString;
exports.whitespace = whitespace;
exports.manyWhitespace = manyWhitespace;
exports.betweenManyWhitespace = betweenManyWhitespace;
exports.doubleQuote = doubleQuote;
exports.lParen = lParen;
exports.rParen = rParen;
exports.lBracket = lBracket;
exports.rBracket = rBracket;
exports.lBrace = lBrace;
exports.rBrace = rBrace;
exports.betweenBrackets = betweenBrackets;
exports.betweenParens = betweenParens;
exports.betweenBraces = betweenBraces;
exports.betweenDoubleQuote = betweenDoubleQuote;
exports.digit = digit;
exports.uppercase = uppercase;
exports.lowercase = lowercase;
exports.stringLiteral = stringLiteral;
exports.numberLiteral = numberLiteral;
exports.identifier = identifier;
exports.literal = literal;
exports.definition = definition;
exports.literalToString = literalToString;
exports.functionToString = functionToString;
exports.blockToString = blockToString;
exports.parser = parser;
/* whitespace Not a pure module */
