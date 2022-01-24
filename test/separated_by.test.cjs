// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Json = require("./json/parser/json.cjs");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Res_parser = require("../src/res_parser.cjs");
var Test_runners = require("./test_runners.cjs");

Test_runners.runTests(Res_parser.map(Res_parser.map(Res_parser.separatedBy1(Res_parser.$$char(/* 'a' */97), Res_parser.$$char(/* ',' */44)), (function (__x) {
                return Belt_List.map(__x, Json.charToString);
              })), Json.concatStringList), (function (param, param$1) {
        return "[Separated by] Simple \"" + param$1[0] + " == " + param$1[1] + "\"";
      }), [
      [
        "a,a",
        "aa",
        ""
      ],
      [
        "a,,a",
        "a",
        ",,a"
      ],
      [
        "a,a,a,a",
        "aaaa",
        ""
      ],
      [
        "a",
        "a",
        ""
      ],
      [
        "a,b,c",
        "a",
        ",b,c"
      ]
    ]);

var manyWhitespace = Res_parser.many(Res_parser.$$char(/* ' ' */32));

var commaWithWhitespace = Res_parser.between(Res_parser.$$char(/* ',' */44), manyWhitespace, manyWhitespace);

Test_runners.runTests(Res_parser.separatedBy1(Res_parser.$$char(/* 'a' */97), commaWithWhitespace), (function (param, param$1) {
        return "[Separated by] semi-simple \"" + param$1[0] + "\"";
      }), [
      [
        "a, a,a,   a    ,a",
        {
          hd: /* 'a' */97,
          tl: {
            hd: /* 'a' */97,
            tl: {
              hd: /* 'a' */97,
              tl: {
                hd: /* 'a' */97,
                tl: {
                  hd: /* 'a' */97,
                  tl: /* [] */0
                }
              }
            }
          }
        },
        ""
      ],
      [
        "a",
        {
          hd: /* 'a' */97,
          tl: /* [] */0
        },
        ""
      ]
    ]);

Test_runners.runTests(Res_parser.map(Res_parser.map(Res_parser.separatedBy(Res_parser.$$char(/* 'a' */97), Res_parser.$$char(/* ',' */44)), (function (__x) {
                return Belt_List.map(__x, Json.charToString);
              })), Json.concatStringList), (function (param, param$1) {
        return "[Separated by many] simple \"" + param$1[0] + "\"";
      }), [
      [
        "a,a",
        "aa",
        ""
      ],
      [
        "a,a,a,a",
        "aaaa",
        ""
      ],
      [
        "a",
        "a",
        ""
      ],
      [
        "a,b,c",
        "a",
        ",b,c"
      ]
    ]);

var atLeastOneComma = Res_parser.atLeastOne(Res_parser.$$char(/* ',' */44));

Test_runners.runTests(Res_parser.map(Res_parser.map(Res_parser.separatedBy(Res_parser.$$char(/* 'a' */97), atLeastOneComma), (function (__x) {
                return Belt_List.map(__x, Json.charToString);
              })), Json.concatStringList), (function (param, param$1) {
        return "[Separated by] at least one \"" + param$1[0] + "\"";
      }), [
      [
        "a,,,a",
        "aa",
        ""
      ],
      [
        "a,,,,,a,,a,a,,,a",
        "aaaaa",
        ""
      ]
    ]);

var commaSeparatedByWhitespace = Res_parser.andThen(Res_parser.andThen(Json.manyWhitespace, Res_parser.$$char(/* ',' */44)), Json.manyWhitespace);

Test_runners.runTests(Res_parser.map(Res_parser.map(Res_parser.separatedBy(Res_parser.$$char(/* 'a' */97), commaSeparatedByWhitespace), (function (__x) {
                return Belt_List.map(__x, Json.charToString);
              })), Json.concatStringList), (function (param, param$1) {
        return "[Separated by ++] at least one \"" + param$1[0] + "\"";
      }), [
      [
        "a, a",
        "aa",
        ""
      ],
      [
        "a      ,a",
        "aa",
        ""
      ],
      [
        "a      ,a,a,a      ,a",
        "aaaaa",
        ""
      ]
    ]);

var P;

exports.P = P;
/*  Not a pure module */
