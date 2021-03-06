// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Json = require("./parser/json.cjs");
var Test_runners = require("../test_runners.cjs");

Test_runners.runTests(Json.parse, (function (input, param) {
        return "[JSON String] \"" + input + "\"";
      }), [
      [
        "\"1\"",
        {
          TAG: /* String */2,
          _0: "1"
        },
        ""
      ],
      [
        "\"hello\"",
        {
          TAG: /* String */2,
          _0: "hello"
        },
        ""
      ],
      [
        "\"   \"",
        {
          TAG: /* String */2,
          _0: "   "
        },
        ""
      ],
      [
        "\"\"",
        {
          TAG: /* String */2,
          _0: ""
        },
        ""
      ],
      [
        "\"\u0050\"",
        {
          TAG: /* String */2,
          _0: "P"
        },
        ""
      ],
      [
        "\"\u0050\u0069\u0061n\u006F\"",
        {
          TAG: /* String */2,
          _0: "Piano"
        },
        ""
      ]
    ]);

/*  Not a pure module */
