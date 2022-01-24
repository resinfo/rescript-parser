// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Ava = require("rescript-ava/src/ava.cjs");
var Json = require("./parser/json.cjs");
var Belt_Range = require("rescript/lib/js/belt_Range.js");
var Res_parser = require("../../src/res_parser.cjs");

function run(param) {
  return Res_parser.run(Json.digit, param);
}

var remaining = Res_parser.State.remaining;

function shouldNotPass(param) {
  return "Should not pass with \"" + param[1] + "\" remaining";
}

Ava.skip("Digit succeeds", (function (t) {
        return Belt_Range.forEach(0, 9, (function (index) {
                      var asString = String(index);
                      var result = Res_parser.run(Json.digit, asString);
                      if (result.TAG !== /* Ok */0) {
                        return Ava.fail(t, "Failure to parse digit: " + result._0.message, undefined);
                      }
                      var match = result._0;
                      var state = match[1];
                      if (Res_parser.State.remaining(state) === "") {
                        return Ava.true_(t, match[0] === asString, undefined, undefined);
                      } else {
                        return Ava.fail(t, "Parsing digit had remaining characters: \"" + state.input + "\"", undefined);
                      }
                    }));
      }));

var P;

var shouldNotFail = "Should not fail";

exports.P = P;
exports.run = run;
exports.remaining = remaining;
exports.shouldNotPass = shouldNotPass;
exports.shouldNotFail = shouldNotFail;
/*  Not a pure module */
