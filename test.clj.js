var hello__HYPHEN__world = 12;

function yes(a) {
  return a;
}

var add = function () {
  return jsadd(1, 2, 3);
};

var hello = (function () {
  var world = "world";
  return world;
})();

function map(arr) {
  return js__HYPHEN__arr__HYPHEN__map(arr, function (i) {
    return add(i, 12);
  });
}

function add2() {
  return (function () {
    var x = 12;
    return x;
  })();
}

var add = (function (a, b) {
  return jsadd(a, b);
})();

var my_obj = {
  hello: "world",
  world: 1,
  "my-key": "sth",
};

var mymod = (() => {
  function x(a) {
    return a;
  }
  return {
    x: x,
  };
})();

module.exports = {
  yes: yes,
  hello: hello,
};
