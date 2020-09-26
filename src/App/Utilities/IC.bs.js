// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var Ix$OptolithClient = require("../../Data/Ix.bs.js");

function getAPCostBaseByIC(ic) {
  switch (ic) {
    case /* A */0 :
        return 1;
    case /* B */1 :
        return 2;
    case /* C */2 :
        return 3;
    case /* D */3 :
        return 4;
    case /* E */4 :
        return 5;
    case /* F */5 :
        return 8;
    case /* G */6 :
        return 10;
    case /* H */7 :
        return 20;
    
  }
}

function getAPForBounds(ic, l, u) {
  var __x = Ix$OptolithClient.range(/* tuple */[
        l + 1 | 0,
        u
      ]);
  return List.fold_right((function (sr) {
                var partial_arg = Caml_int32.imul(getAPCostBaseByIC(ic), sr);
                return (function (param) {
                    return partial_arg + param | 0;
                  });
              }), __x, 0);
}

function getAPForRange(ic, fromSR, toSR) {
  if (fromSR < toSR) {
    return getAPForBounds(ic, fromSR, toSR);
  } else if (fromSR > toSR) {
    return -getAPForBounds(ic, toSR, fromSR) | 0;
  } else {
    return 0;
  }
}

function getAPForInc(ic, fromSR) {
  return Caml_int32.imul(getAPCostBaseByIC(ic), fromSR + 1 | 0);
}

function getAPForDec(ic, fromSR) {
  return -Caml_int32.imul(getAPCostBaseByIC(ic), fromSR) | 0;
}

function icToStr(ic) {
  switch (ic) {
    case /* A */0 :
        return "A";
    case /* B */1 :
        return "B";
    case /* C */2 :
        return "C";
    case /* D */3 :
        return "D";
    case /* E */4 :
        return "E";
    case /* F */5 :
        return "F";
    case /* G */6 :
        return "G";
    case /* H */7 :
        return "H";
    
  }
}

function icToIx(ic) {
  return ic;
}

function t(json) {
  var x = Json_decode.string(json);
  switch (x) {
    case "A" :
        return /* A */0;
    case "B" :
        return /* B */1;
    case "C" :
        return /* C */2;
    case "D" :
        return /* D */3;
    case "E" :
        return /* E */4;
    case "F" :
        return /* F */5;
    case "G" :
        return /* G */6;
    case "H" :
        return /* H */7;
    default:
      throw [
            Json_decode.DecodeError,
            "Unknown improvement cost: " + x
          ];
  }
}

var Decode = {
  t: t
};

var getAPForActivatation = getAPCostBaseByIC;

exports.getAPForRange = getAPForRange;
exports.getAPForInc = getAPForInc;
exports.getAPForDec = getAPForDec;
exports.getAPForActivatation = getAPForActivatation;
exports.icToStr = icToStr;
exports.icToIx = icToIx;
exports.Decode = Decode;
/* No side effect */
