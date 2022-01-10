// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");

function listLast(_l) {
  while(true) {
    var l = _l;
    if (!l) {
      return ;
    }
    var rest = l.tl;
    if (!rest) {
      return Caml_option.some(l.hd);
    }
    _l = rest;
    continue ;
  };
}

var partial_arg = /-/g;

function stringifyIdentifier(param) {
  return param.replace(partial_arg, "__HYPHEN__");
}

function tab(x) {
  return "".repeat((x << 1));
}

function stringifyDef(def, indent) {
  var newline = "\n";
  switch (def.TAG | 0) {
    case /* DVariable */0 :
        return "var " + stringifyIdentifier(def._0) + " = " + stringifyLiteral(def._1, indent + 1 | 0);
    case /* DFunction */1 :
        return stringifyFunction(def._0, def._1, def._2, indent + 1 | 0);
    case /* DModule */2 :
        var match = def._0;
        var defs = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(match._1, (function (def) {
                        return stringifyDef(def, indent + 1 | 0);
                      }))), "", (function (x) {
                return newline + x;
              }));
        var $$exports = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(match._2, (function (param) {
                        return param[0] + ": " + stringifyLiteral(param[1], indent);
                      }))), "", (function (x) {
                return x + ",";
              }));
        return newline + ("var " + match._0 + " = (() => {") + defs + newline + "return {" + newline + $$exports + newline + newline + "}" + newline + "})();";
    case /* DExport */3 :
        return "";
    
  }
}

function stringifyFunction(identifier, args, literal, indent) {
  var ws = tab(indent);
  var newline = "\n" + ws + "";
  var args$1 = Belt_Array.joinWith(Belt_List.toArray(args), ",", (function (x) {
          return x;
        }));
  return "function " + stringifyIdentifier(identifier) + "(" + args$1 + ") {" + newline + tab(indent + 1 | 0) + "return " + stringifyLiteral(literal, indent + 1 | 0) + ";" + newline + "}";
}

function stringifyLiteral(literal, indent) {
  var newline = "\n";
  if (typeof literal === "number") {
    if (literal === /* LTrue */0) {
      return "true";
    } else {
      return "false";
    }
  }
  switch (literal.TAG | 0) {
    case /* LIdentifier */0 :
    case /* LNumber */1 :
        return literal._0;
    case /* LString */2 :
        return "\"" + literal._0 + "\"";
    case /* LArray */3 :
        var body = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(literal._0, (function (x) {
                        return stringifyLiteral(x, indent);
                      }))), ", ", (function (x) {
                return x;
              }));
        return "[" + body + "]";
    case /* LRecord */4 :
        return [
                  "{",
                  Belt_Array.joinWith(Belt_List.toArray(literal._0), ",\n", (function (param) {
                          return "\"" + param[0] + "\": " + stringifyLiteral(param[1], indent + 1 | 0);
                        })),
                  "}"
                ].join("\n");
    case /* LLambda */5 :
        var params = Belt_List.toArray(literal._0).join(", ");
        return "function(" + params + ") {" + newline + tab(indent) + "return " + stringifyLiteral(literal._1, indent) + newline + "}";
    case /* LExecution */6 :
        var $$function = literal._0;
        if ($$function.TAG === /* FNamed */0) {
          return stringifyIdentifier($$function._0) + "(" + Belt_Array.joinWith(Belt_List.toArray(literal._1), ", ", (function (__x) {
                        return stringifyLiteral(__x, indent);
                      })) + ")";
        }
        var body$1 = stringifyFunction("", $$function._0, $$function._1, indent + 1 | 0);
        return "(" + body$1 + ")();";
    case /* LBlock */7 :
        var bodies = Belt_Array.joinWith(Belt_Array.map(Belt_List.toArray(literal._0), (function (x) {
                    switch (x.TAG | 0) {
                      case /* BVariable */0 :
                          return "var " + x._0 + " = " + stringifyLiteral(x._1, indent + 1 | 0) + ";";
                      case /* BFunction */1 :
                          return "(" + stringifyFunction(x._0, x._1, x._2, indent + 1 | 0) + ")();";
                      case /* BLiteral */2 :
                          return stringifyLiteral(x._0, indent + 1 | 0);
                      
                    }
                  })), "\n", (function (x) {
                return x;
              }));
        var $$return = Belt_Option.getWithDefault(Belt_Option.map(literal._1, (function (x) {
                    return "return " + stringifyLiteral(x, indent + 1 | 0);
                  })), "");
        return "(function() {" + bodies + " " + $$return + "; })()";
    
  }
}

function stringifyBlock(block, indent) {
  switch (block.TAG | 0) {
    case /* BVariable */0 :
        return "var " + block._0 + " = " + stringifyLiteral(block._1, indent + 1 | 0) + ";";
    case /* BFunction */1 :
        return "(" + stringifyFunction(block._0, block._1, block._2, indent + 1 | 0) + ")();";
    case /* BLiteral */2 :
        return stringifyLiteral(block._0, indent + 1 | 0);
    
  }
}

function setBlockName(name, block) {
  switch (block.TAG | 0) {
    case /* BVariable */0 :
        return {
                TAG: /* BVariable */0,
                _0: name + block._0,
                _1: block._1
              };
    case /* BFunction */1 :
        return {
                TAG: /* BFunction */1,
                _0: name + block._0,
                _1: block._1,
                _2: block._2
              };
    case /* BLiteral */2 :
        return {
                TAG: /* BLiteral */2,
                _0: block._0
              };
    
  }
}

function generate(t) {
  var file = Belt_Array.joinWith(Belt_List.toArray(Belt_List.map(t._1, (function (def) {
                  return stringifyDef(def, 0);
                }))), "\n\n", (function (x) {
          return x;
        }));
  var $$exports = Belt_Array.joinWith(Belt_List.toArray(t._2), ",\n", (function (param) {
          return tab(1) + (param[0] + ": " + stringifyLiteral(param[1], 0));
        }));
  var $$exports$1 = "module.exports = {\n" + $$exports + "\n}";
  return [
          t._0,
          file + "\n\n" + $$exports$1
        ];
}

var joinWith = Belt_Array.joinWith;

exports.listLast = listLast;
exports.stringifyIdentifier = stringifyIdentifier;
exports.tab = tab;
exports.joinWith = joinWith;
exports.stringifyDef = stringifyDef;
exports.stringifyLiteral = stringifyLiteral;
exports.stringifyFunction = stringifyFunction;
exports.stringifyBlock = stringifyBlock;
exports.setBlockName = setBlockName;
exports.generate = generate;
/* No side effect */
