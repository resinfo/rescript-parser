// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Ava = require("rescript-ava/src/ava.cjs");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Res_parser = require("../src/res_parser.cjs");

Ava.test("Error position", (function (t) {
        var parser = Res_parser.string("hello world");
        var error = Res_parser.run(parser, "hel1o world");
        if (error.TAG === /* Ok */0) {
          Ava.fail(t, undefined, undefined);
        } else {
          var match = error._0.state;
          var position = match.position;
          Ava.is(t, position.col, 4, undefined, undefined);
          Ava.is(t, position.index, 3, undefined, undefined);
          Ava.is(t, position.lineStart, 0, undefined, undefined);
          Ava.is(t, position.line, 1, undefined, undefined);
          Ava.true_(t, position.prevLines === /* [] */0, undefined, undefined);
        }
        var parser$1 = Res_parser.andThen(Res_parser.keepLeft(Res_parser.$$char(/* 'a' */97), Res_parser.many(Res_parser.anyOf([
                          /* ' ' */32,
                          /* '\n' */10
                        ]))), Res_parser.$$char(/* 'b' */98));
        var error$1 = Res_parser.run(parser$1, "a\nc");
        if (error$1.TAG === /* Ok */0) {
          return Ava.fail(t, undefined, undefined);
        }
        var match$1 = error$1._0.state;
        var position$1 = match$1.position;
        Ava.is(t, position$1.col, 1, undefined, undefined);
        Ava.is(t, position$1.index, 2, undefined, undefined);
        Ava.is(t, position$1.lineStart, 2, undefined, undefined);
        Ava.is(t, position$1.line, 2, undefined, undefined);
        return Ava.true_(t, Caml_obj.caml_equal(position$1.prevLines, {
                        hd: [
                          0,
                          1
                        ],
                        tl: /* [] */0
                      }), undefined, undefined);
      }));

var P;

exports.P = P;
/*  Not a pure module */
