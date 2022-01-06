// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Ava = require("rescript-ava/src/ava.cjs");
var Json = require("./json.cjs");
var Parser = require("../../src/parser.cjs");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

function run(param) {
  return Parser.run(Json.json, param);
}

var okInputs = [
  [
    "Empty Object",
    "{}",
    {
      TAG: /* Object */0,
      _0: /* [] */0
    }
  ],
  [
    "Empty Object with padding",
    "    {  }       ",
    {
      TAG: /* Object */0,
      _0: /* [] */0
    }
  ],
  [
    "Object with number attribute",
    " { \"a\":1  }       ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "a",
          {
            TAG: /* Number */1,
            _0: "1"
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with empty key",
    " { \"\":\"\"  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "",
          {
            TAG: /* String */2,
            _0: ""
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Nested Object",
    " { \"foo\": {\"bar  \"   : { \"baz\":1 }}  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Object */0,
            _0: {
              hd: [
                "bar  ",
                {
                  TAG: /* Object */0,
                  _0: {
                    hd: [
                      "baz",
                      {
                        TAG: /* Number */1,
                        _0: "1"
                      }
                    ],
                    tl: /* [] */0
                  }
                }
              ],
              tl: /* [] */0
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with array values 1",
    " { \"foo\": [1]  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1"
              },
              tl: /* [] */0
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with nested array values 1",
    " { \"foo\": [1, [[2]]]  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1"
              },
              tl: {
                hd: {
                  TAG: /* Array */3,
                  _0: {
                    hd: {
                      TAG: /* Array */3,
                      _0: {
                        hd: {
                          TAG: /* Number */1,
                          _0: "2"
                        },
                        tl: /* [] */0
                      }
                    },
                    tl: /* [] */0
                  }
                },
                tl: /* [] */0
              }
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with nested array values 2",
    " { \"foo\": [1, [[2, 2]]]  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1"
              },
              tl: {
                hd: {
                  TAG: /* Array */3,
                  _0: {
                    hd: {
                      TAG: /* Array */3,
                      _0: {
                        hd: {
                          TAG: /* Number */1,
                          _0: "2"
                        },
                        tl: {
                          hd: {
                            TAG: /* Number */1,
                            _0: "2"
                          },
                          tl: /* [] */0
                        }
                      }
                    },
                    tl: /* [] */0
                  }
                },
                tl: /* [] */0
              }
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with nested array values 3",
    " { \"foo\": [1, [[2, 2, []]]]  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1"
              },
              tl: {
                hd: {
                  TAG: /* Array */3,
                  _0: {
                    hd: {
                      TAG: /* Array */3,
                      _0: {
                        hd: {
                          TAG: /* Number */1,
                          _0: "2"
                        },
                        tl: {
                          hd: {
                            TAG: /* Number */1,
                            _0: "2"
                          },
                          tl: {
                            hd: {
                              TAG: /* Array */3,
                              _0: /* [] */0
                            },
                            tl: /* [] */0
                          }
                        }
                      }
                    },
                    tl: /* [] */0
                  }
                },
                tl: /* [] */0
              }
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with nested array values 4",
    " { \"foo\": [1, [[2   ,2,[[3         ]]] ]]  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1"
              },
              tl: {
                hd: {
                  TAG: /* Array */3,
                  _0: {
                    hd: {
                      TAG: /* Array */3,
                      _0: {
                        hd: {
                          TAG: /* Number */1,
                          _0: "2"
                        },
                        tl: {
                          hd: {
                            TAG: /* Number */1,
                            _0: "2"
                          },
                          tl: {
                            hd: {
                              TAG: /* Array */3,
                              _0: {
                                hd: {
                                  TAG: /* Array */3,
                                  _0: {
                                    hd: {
                                      TAG: /* Number */1,
                                      _0: "3"
                                    },
                                    tl: /* [] */0
                                  }
                                },
                                tl: /* [] */0
                              }
                            },
                            tl: /* [] */0
                          }
                        }
                      }
                    },
                    tl: /* [] */0
                  }
                },
                tl: /* [] */0
              }
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ],
  [
    "Object with nested arrays and objects",
    " { \"foo\": [1, [[2   ,2,[[3        , {\"b_\": [1234]} ]]] ]]  }  ",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "foo",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1"
              },
              tl: {
                hd: {
                  TAG: /* Array */3,
                  _0: {
                    hd: {
                      TAG: /* Array */3,
                      _0: {
                        hd: {
                          TAG: /* Number */1,
                          _0: "2"
                        },
                        tl: {
                          hd: {
                            TAG: /* Number */1,
                            _0: "2"
                          },
                          tl: {
                            hd: {
                              TAG: /* Array */3,
                              _0: {
                                hd: {
                                  TAG: /* Array */3,
                                  _0: {
                                    hd: {
                                      TAG: /* Number */1,
                                      _0: "3"
                                    },
                                    tl: {
                                      hd: {
                                        TAG: /* Object */0,
                                        _0: {
                                          hd: [
                                            "b_",
                                            {
                                              TAG: /* Array */3,
                                              _0: {
                                                hd: {
                                                  TAG: /* Number */1,
                                                  _0: "1234"
                                                },
                                                tl: /* [] */0
                                              }
                                            }
                                          ],
                                          tl: /* [] */0
                                        }
                                      },
                                      tl: /* [] */0
                                    }
                                  }
                                },
                                tl: /* [] */0
                              }
                            },
                            tl: /* [] */0
                          }
                        }
                      }
                    },
                    tl: /* [] */0
                  }
                },
                tl: /* [] */0
              }
            }
          }
        ],
        tl: /* [] */0
      }
    }
  ]
];

Belt_Array.forEach(okInputs, (function (param) {
        var expected = param[2];
        var input = param[1];
        return Ava.test("[JSON] " + param[0] + " succeeds", (function (t) {
                      var err = Parser.run(Json.json, input);
                      if (err.TAG !== /* Ok */0) {
                        return Ava.fail(t, "Shouldn't fail with \"" + err._0 + "\"", undefined);
                      }
                      var match = err._0;
                      var rest = match[1];
                      if (rest === "" && Caml_obj.caml_equal(match[0], expected)) {
                        return Ava.pass(t, undefined, undefined);
                      }
                      return Ava.fail(t, "Shouldn't succeed with \"" + rest + "\" remaining", undefined);
                    }));
      }));

var partiallyOkInputs = [
  [
    "Dangling char",
    "{}a",
    {
      TAG: /* Object */0,
      _0: /* [] */0
    },
    "a"
  ],
  [
    "Dangling closing brackets",
    "{ \"hello\": 1234 }}}",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "hello",
          {
            TAG: /* Number */1,
            _0: "1234"
          }
        ],
        tl: /* [] */0
      }
    },
    "}}"
  ],
  [
    "Dangling closing braces",
    "{ \"hello\": 1234 }]]",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "hello",
          {
            TAG: /* Number */1,
            _0: "1234"
          }
        ],
        tl: /* [] */0
      }
    },
    "]]"
  ],
  [
    "Dangling closing sign",
    "{ \"hello\": [1234] }-",
    {
      TAG: /* Object */0,
      _0: {
        hd: [
          "hello",
          {
            TAG: /* Array */3,
            _0: {
              hd: {
                TAG: /* Number */1,
                _0: "1234"
              },
              tl: /* [] */0
            }
          }
        ],
        tl: /* [] */0
      }
    },
    "-"
  ]
];

Belt_Array.forEach(partiallyOkInputs, (function (param) {
        var remaining = param[3];
        var expected = param[2];
        var input = param[1];
        return Ava.test("[JSON] " + param[0] + " partially succeeds", (function (t) {
                      var err = Parser.run(Json.json, input);
                      if (err.TAG !== /* Ok */0) {
                        return Ava.fail(t, "Shouldn't fail with \"" + err._0 + "\"", undefined);
                      }
                      var match = err._0;
                      var rest = match[1];
                      var output = match[0];
                      if (Caml_obj.caml_equal(output, expected) && remaining === rest) {
                        return Ava.pass(t, "Should succeed with \"" + rest + "\" remaining", undefined);
                      } else {
                        return Ava.fail(t, "Shouldn't succeed with \"" + Json.toString(output) + "\" and \"" + rest + "\" remaining; expected \"" + Json.toString(expected) + "\" and \"" + remaining + "\" remaining", undefined);
                      }
                    }));
      }));

var erroneousInputs = [
  "       +{}",
  "_{}",
  "|{\"hello\": 1}",
  "a{}",
  "~{f}"
];

Belt_Array.forEachWithIndex(erroneousInputs, (function (index, input) {
        return Ava.test("[JSON] Object " + String(index) + " fails", (function (t) {
                      var err = Parser.run(Json.json, input);
                      if (err.TAG === /* Ok */0) {
                        return Ava.fail(t, "Shouldn't succeed with \"" + err._0[1] + "\" remaining", undefined);
                      } else {
                        return Ava.pass(t, "Should fail with \"" + err._0 + "\"", undefined);
                      }
                    }));
      }));

var P;

exports.P = P;
exports.run = run;
exports.okInputs = okInputs;
exports.partiallyOkInputs = partiallyOkInputs;
exports.erroneousInputs = erroneousInputs;
/*  Not a pure module */