// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");

function maybe(decode, json) {
  if (json === undefined || json === null) {
    return /* Nothing */0;
  } else {
    return /* Just */[Curry._1(decode, json)];
  }
}

function optionalField(key, decode, json) {
  return Json_decode.field(key, (function (param) {
                return maybe(decode, param);
              }), json);
}

function $$const(x, json) {
  if (Caml_obj.caml_equal(json, x)) {
    return x;
  } else {
    throw [
          Json_decode.DecodeError,
          "Expected \"" + (JSON.stringify(json) + ("\", but received: " + JSON.stringify(json)))
        ];
  }
}

exports.maybe = maybe;
exports.optionalField = optionalField;
exports.$$const = $$const;
/* No side effect */