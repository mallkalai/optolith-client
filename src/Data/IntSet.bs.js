// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var $$Set = require("bs-platform/lib/js/set.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_int = require("bs-platform/lib/js/js_int.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Maybe$OptolithClient = require("./Maybe.bs.js");
var Function$OptolithClient = require("./Function.bs.js");

var compare = Caml_obj.caml_compare;

var IntSet = $$Set.Make({
      compare: compare
    });

function foldr(f, initial, s) {
  return Curry._3(IntSet.fold, f, s, initial);
}

function foldl(f, initial, s) {
  return Curry._3(IntSet.fold, (function (param, param$1) {
                return Function$OptolithClient.flip(f, param, param$1);
              }), s, initial);
}

var toList = IntSet.elements;

var length = IntSet.cardinal;

function elem(x) {
  return Curry._1(IntSet.exists, (function (param) {
                return x === param;
              }));
}

function sum(param) {
  return Curry._3(IntSet.fold, (function (prim, prim$1) {
                return prim + prim$1 | 0;
              }), param, 0);
}

function product(param) {
  return Curry._3(IntSet.fold, Caml_int32.imul, param, 1);
}

function maximum(param) {
  return Curry._3(IntSet.fold, (function (prim, prim$1) {
                return Math.max(prim, prim$1);
              }), param, Js_int.min);
}

function minimum(param) {
  return Curry._3(IntSet.fold, (function (prim, prim$1) {
                return Math.min(prim, prim$1);
              }), param, Js_int.max);
}

function concatMap(f, s) {
  return Curry._3(IntSet.fold, (function (x, acc) {
                return Curry._2(IntSet.union, acc, Curry._1(f, x));
              }), s, IntSet.empty);
}

function any(pred, s) {
  return !Curry._2(IntSet.for_all, (function (x) {
                return !Curry._1(pred, x);
              }), s);
}

function all(pred) {
  return Curry._1(IntSet.for_all, Curry.__1(pred));
}

function notElem(x, s) {
  return !Curry._2(IntSet.exists, (function (param) {
                return x === param;
              }), s);
}

function find(pred, s) {
  return Maybe$OptolithClient.optionToMaybe(Curry._2(IntSet.find_first_opt, pred, s));
}

var Foldable_null = IntSet.is_empty;

var Foldable = {
  foldr: foldr,
  foldl: foldl,
  toList: toList,
  $$null: Foldable_null,
  length: length,
  elem: elem,
  sum: sum,
  product: product,
  maximum: maximum,
  minimum: minimum,
  concatMap: concatMap,
  any: any,
  all: all,
  notElem: notElem,
  find: find
};

var insert = IntSet.add;

var $$delete = IntSet.remove;

function toggle(x, s) {
  if (Curry._2(IntSet.exists, (function (param) {
            return x === param;
          }), s)) {
    return Curry._2($$delete, x, s);
  } else {
    return Curry._2(insert, x, s);
  }
}

var empty = IntSet.empty;

var singleton = IntSet.singleton;

var fromList = IntSet.of_list;

var member = elem;

var notMember = notElem;

var size = length;

var union = IntSet.union;

var difference = IntSet.diff;

var filter = IntSet.filter;

var map = IntSet.map;

var elems = toList;

exports.Foldable = Foldable;
exports.empty = empty;
exports.singleton = singleton;
exports.fromList = fromList;
exports.insert = insert;
exports.$$delete = $$delete;
exports.toggle = toggle;
exports.member = member;
exports.notMember = notMember;
exports.size = size;
exports.union = union;
exports.difference = difference;
exports.filter = filter;
exports.map = map;
exports.elems = elems;
/* IntSet Not a pure module */
