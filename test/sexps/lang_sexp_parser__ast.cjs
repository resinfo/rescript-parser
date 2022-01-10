// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

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

function definitionToString(definition) {
  switch (definition.TAG | 0) {
    case /* DVariable */0 :
        return "DVariable(" + definition._0 + ")";
    case /* DFunction */1 :
        return "DFunction(" + definition._0 + ", " + Belt_Array.joinWith(Belt_List.toArray(definition._1), ", ", (function (x) {
                      return x;
                    })) + ")";
    case /* DModule */2 :
        return "DModule(" + definition._0._0 + ")";
    case /* DExport */3 :
        return "DExport(" + definition._0 + ", " + literalToString(definition._1) + ")";
    
  }
}

var P;

exports.P = P;
exports.literalToString = literalToString;
exports.functionToString = functionToString;
exports.blockToString = blockToString;
exports.definitionToString = definitionToString;
/* No side effect */
