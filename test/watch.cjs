// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Curry = require("rescript/lib/js/curry.js");
var Parser = require("../src/parser.cjs");
var Prettier = require("prettier");
var Lang_sexp_parser = require("./sexps/lang_sexp_parser.cjs");
var Lang_sexp_generator = require("./sexps/lang_sexp_generator.cjs");

function time(name) {
  console.time(name);
  return function (param) {
    console.timeEnd(name);
    
  };
}

console.clear();

console.log("File changed");

var timeRead = time("[Read file]");

var file = Fs.readFileSync("test.clj", "utf8");

timeRead(undefined);

var timeGenerate = time("[Generate]");

var input = "(module test {" + file + "})";

var output = Parser.run(Lang_sexp_parser.parser, input);

timeGenerate(undefined);

var match;

if (output.TAG === /* Ok */0) {
  var match$1 = Lang_sexp_generator.generate(output._0[0]);
  var timeFormat = time("[Format]");
  var contents = Prettier.format(match$1[1], {
        parser: "babel"
      });
  Curry._1(timeFormat, undefined);
  match = [
    match$1[0],
    contents
  ];
} else {
  match = [
    "test",
    output._0
  ];
}

var contents$1 = match[1];

var name = match[0];

var timeWrite = time("[Time write]");

Fs.writeFileSync(name + ".clj.js", contents$1, "utf8");

timeWrite(undefined);

exports.time = time;
exports.timeRead = timeRead;
exports.file = file;
exports.timeGenerate = timeGenerate;
exports.input = input;
exports.output = output;
exports.name = name;
exports.contents = contents$1;
exports.timeWrite = timeWrite;
/*  Not a pure module */
