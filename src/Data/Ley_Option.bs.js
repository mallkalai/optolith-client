// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Ley_Function$OptolithClient = require("./Ley_Function.bs.js");

function $less$$great(f, mx) {
  if (mx !== undefined) {
    return Caml_option.some(Curry._1(f, Caml_option.valFromOption(mx)));
  }
  
}

function $less$amp$great(mx, f) {
  return $less$$great(f, mx);
}

function $less$star$great(mf, mx) {
  if (mf !== undefined) {
    return $less$$great(mf, mx);
  }
  
}

var Applicative = {
  $less$star$great: $less$star$great,
  ap: $less$star$great
};

function $less$pipe$great(mx, my) {
  if (mx !== undefined) {
    return mx;
  } else {
    return my;
  }
}

function guard(pred) {
  if (pred) {
    return /* () */0;
  }
  
}

function $$return(x) {
  return Caml_option.some(x);
}

function $great$great$eq(mx, f) {
  if (mx !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(mx));
  }
  
}

function $eq$less$less(f, mx) {
  return $great$great$eq(mx, f);
}

function $great$great(x, y) {
  return $great$great$eq(x, (function (param) {
                return Ley_Function$OptolithClient.$$const(y, param);
              }));
}

function mapM(f, xs) {
  if (xs) {
    var match = Curry._1(f, xs[0]);
    if (match !== undefined) {
      var z = Caml_option.valFromOption(match);
      return $less$$great((function (zs) {
                    return /* :: */[
                            z,
                            zs
                          ];
                  }), mapM(f, xs[1]));
    } else {
      return ;
    }
  } else {
    return /* [] */0;
  }
}

function $great$eq$great(f, g, x) {
  return $great$great$eq(Curry._1(f, x), g);
}

function join(x) {
  return $great$great$eq(x, Ley_Function$OptolithClient.id);
}

function liftM2(f, mx, my) {
  return $great$great$eq(mx, (function (x) {
                return $less$$great(Curry._1(f, x), my);
              }));
}

function liftM3(f, mx, my, mz) {
  return $great$great$eq(mx, (function (x) {
                return $great$great$eq(my, (function (y) {
                              return $less$$great(Curry._2(f, x, y), mz);
                            }));
              }));
}

function liftM4(f, mx, my, mz, ma) {
  return $great$great$eq(mx, (function (x) {
                return $great$great$eq(my, (function (y) {
                              return $great$great$eq(mz, (function (z) {
                                            return $less$$great(Curry._3(f, x, y, z), ma);
                                          }));
                            }));
              }));
}

function foldr(f, init, mx) {
  if (mx !== undefined) {
    return Curry._2(f, Caml_option.valFromOption(mx), init);
  } else {
    return init;
  }
}

function foldl(f, init, mx) {
  if (mx !== undefined) {
    return Curry._2(f, init, Caml_option.valFromOption(mx));
  } else {
    return init;
  }
}

function toList(mx) {
  if (mx !== undefined) {
    return /* :: */[
            Caml_option.valFromOption(mx),
            /* [] */0
          ];
  } else {
    return /* [] */0;
  }
}

function length(mx) {
  if (mx !== undefined) {
    return 1;
  } else {
    return 0;
  }
}

function elem(e, mx) {
  if (mx !== undefined) {
    return e === Caml_option.valFromOption(mx);
  } else {
    return false;
  }
}

function sum(mx) {
  if (mx !== undefined) {
    return mx;
  } else {
    return 0;
  }
}

function product(mx) {
  if (mx !== undefined) {
    return mx;
  } else {
    return 1;
  }
}

function concat(mxs) {
  if (mxs !== undefined) {
    return mxs;
  } else {
    return /* [] */0;
  }
}

function concatMap(f, mx) {
  if (mx !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(mx));
  } else {
    return /* [] */0;
  }
}

function con(mx) {
  if (mx !== undefined) {
    return mx;
  } else {
    return true;
  }
}

function dis(mx) {
  if (mx !== undefined) {
    return mx;
  } else {
    return false;
  }
}

function any(pred, mx) {
  if (mx !== undefined) {
    return Curry._1(pred, Caml_option.valFromOption(mx));
  } else {
    return false;
  }
}

function all(pred, mx) {
  if (mx !== undefined) {
    return Curry._1(pred, Caml_option.valFromOption(mx));
  } else {
    return true;
  }
}

function notElem(e, mx) {
  return !elem(e, mx);
}

function find(pred, mx) {
  if (mx !== undefined) {
    var x = Caml_option.valFromOption(mx);
    if (Curry._1(pred, x)) {
      return Caml_option.some(x);
    } else {
      return ;
    }
  }
  
}

function sappend(mxs, mys) {
  if (mxs !== undefined && mys !== undefined) {
    return List.append(mxs, mys);
  } else {
    return mxs;
  }
}

var Semigroup = {
  sappend: sappend
};

function isSome(m) {
  return m !== undefined;
}

function isNone(m) {
  return m === undefined;
}

function fromSome(param) {
  if (param !== undefined) {
    return Caml_option.valFromOption(param);
  } else {
    return Pervasives.invalid_arg("Cannot unwrap None.");
  }
}

function fromOption(def, mx) {
  if (mx !== undefined) {
    return Caml_option.valFromOption(mx);
  } else {
    return def;
  }
}

function option(def, f, mx) {
  if (mx !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(mx));
  } else {
    return def;
  }
}

function listToOption(xs) {
  if (xs) {
    return Caml_option.some(xs[0]);
  }
  
}

function catOptions(xs) {
  return List.fold_right((function (param) {
                return option(Ley_Function$OptolithClient.id, (function (x, xs) {
                              return /* :: */[
                                      x,
                                      xs
                                    ];
                            }), param);
              }), xs, /* [] */0);
}

function mapOption(f, xs) {
  return List.fold_right((function (param) {
                return Ley_Function$OptolithClient.$less$neg((function (param) {
                              return option(Ley_Function$OptolithClient.id, (function (x, xs) {
                                            return /* :: */[
                                                    x,
                                                    xs
                                                  ];
                                          }), param);
                            }), f, param);
              }), xs, /* [] */0);
}

function ensure(pred, x) {
  if (Curry._1(pred, x)) {
    return Caml_option.some(x);
  }
  
}

function imapOptionAux(f, _index, _xs) {
  while(true) {
    var xs = _xs;
    var index = _index;
    if (xs) {
      var xs$1 = xs[1];
      var match = Curry._2(f, index, xs[0]);
      if (match !== undefined) {
        return /* :: */[
                Caml_option.valFromOption(match),
                imapOptionAux(f, index + 1 | 0, xs$1)
              ];
      } else {
        _xs = xs$1;
        _index = index + 1 | 0;
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function imapOption(f, xs) {
  return imapOptionAux(f, 0, xs);
}

var Functor = {
  $less$$great: $less$$great,
  $less$amp$great: $less$amp$great
};

var Alternative = {
  $less$pipe$great: $less$pipe$great,
  guard: guard
};

var Monad = {
  $$return: $$return,
  $great$great$eq: $great$great$eq,
  $eq$less$less: $eq$less$less,
  $great$great: $great$great,
  mapM: mapM,
  $great$eq$great: $great$eq$great,
  join: join,
  liftM2: liftM2,
  liftM3: liftM3,
  liftM4: liftM4
};

var Foldable = {
  foldr: foldr,
  foldl: foldl,
  toList: toList,
  length: length,
  elem: elem,
  sum: sum,
  product: product,
  concat: concat,
  concatMap: concatMap,
  con: con,
  dis: dis,
  any: any,
  all: all,
  notElem: notElem,
  find: find
};

var optionToList = toList;

exports.Functor = Functor;
exports.Applicative = Applicative;
exports.Alternative = Alternative;
exports.Monad = Monad;
exports.Foldable = Foldable;
exports.Semigroup = Semigroup;
exports.isSome = isSome;
exports.isNone = isNone;
exports.fromSome = fromSome;
exports.fromOption = fromOption;
exports.option = option;
exports.listToOption = listToOption;
exports.optionToList = optionToList;
exports.catOptions = catOptions;
exports.mapOption = mapOption;
exports.ensure = ensure;
exports.imapOption = imapOption;
/* No side effect */