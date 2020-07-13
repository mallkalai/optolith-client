// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Ley_List$OptolithClient from "../Data/Ley_List.bs.js";
import * as Ley_IntMap$OptolithClient from "../Data/Ley_IntMap.bs.js";
import * as Ley_Option$OptolithClient from "../Data/Ley_Option.bs.js";
import * as Ley_Function$OptolithClient from "../Data/Ley_Function.bs.js";
import * as SelectOption$OptolithClient from "./SelectOption.bs.js";
import * as Activatable_Convert$OptolithClient from "./Activatable_Convert.bs.js";
import * as Activatable_Accessors$OptolithClient from "./Activatable_Accessors.bs.js";

function getSelectOption(x, id) {
  var partial_arg = Activatable_Accessors$OptolithClient.selectOptions(x);
  return Ley_Option$OptolithClient.Monad.$great$great$eq(Activatable_Convert$OptolithClient.activatableOptionToSelectOptionId(id), (function (param) {
                return Ley_Function$OptolithClient.flip(SelectOption$OptolithClient.SelectOptionMap.lookup, partial_arg, param);
              }));
}

function getSelectOptionName(x, id) {
  return Ley_Option$OptolithClient.Monad.$less$amp$great(getSelectOption(x, id), (function (y) {
                return y.name;
              }));
}

function getSelectOptionCost(x, id) {
  return Ley_Option$OptolithClient.Monad.$great$great$eq(getSelectOption(x, id), (function (y) {
                return y.cost;
              }));
}

function getActiveOptions1(x) {
  return Ley_Option$OptolithClient.mapOption((function (y) {
                return Ley_Option$OptolithClient.listToOption(y.options);
              }), x.active);
}

function mapActiveOptions1(f, x) {
  return Ley_Option$OptolithClient.mapOption((function (y) {
                return Ley_Option$OptolithClient.Monad.$great$great$eq(Ley_Option$OptolithClient.listToOption(y.options), f);
              }), x.active);
}

function getActiveSelectOptions1(param) {
  return mapActiveOptions1(Activatable_Convert$OptolithClient.activatableOptionToSelectOptionId, param);
}

function getActiveOptions2(param) {
  var index = 1;
  return Ley_Option$OptolithClient.mapOption((function (y) {
                return Ley_List$OptolithClient.Safe.atMay(y.options, index);
              }), param.active);
}

function getActiveOptions2Map(x) {
  return Ley_List$OptolithClient.Foldable.foldr((function (current, mp) {
                return Ley_Option$OptolithClient.fromOption(mp, Ley_Option$OptolithClient.Monad.liftM2((function (secondOption, option) {
                                  return Curry._3(SelectOption$OptolithClient.SelectOptionMap.alter, (function (maybeSecondOptions) {
                                                return Ley_List$OptolithClient.cons(secondOption, Ley_Option$OptolithClient.fromOption(/* [] */0, maybeSecondOptions));
                                              }), option, mp);
                                }), Ley_Function$OptolithClient.flip(Ley_List$OptolithClient.Safe.atMay, 1, current.options), Ley_Option$OptolithClient.Monad.$great$great$eq(Ley_Option$OptolithClient.listToOption(current.options), Activatable_Convert$OptolithClient.activatableOptionToSelectOptionId)));
              }), SelectOption$OptolithClient.SelectOptionMap.empty, x.active);
}

function getOption(index, heroEntry) {
  return Ley_List$OptolithClient.Safe.atMay(heroEntry.options, index);
}

function getOption1(heroEntry) {
  return Ley_Option$OptolithClient.listToOption(heroEntry.options);
}

function getOption2(param) {
  return getOption(1, param);
}

function getOption3(param) {
  return getOption(2, param);
}

function getCustomInput(option) {
  if (option.HASH >= 931971705) {
    return option.VAL;
  }
  
}

function getGenericId(option) {
  if (option.HASH !== 61643255) {
    return ;
  } else {
    return option.VAL;
  }
}

function lookupMap(k, mp, f) {
  return Ley_Option$OptolithClient.Monad.$less$$great(f, Curry._2(Ley_IntMap$OptolithClient.lookup, k, mp));
}

function getSkillFromOption(staticData, option) {
  if (option.HASH !== 290194801) {
    return ;
  } else {
    return Curry._2(Ley_IntMap$OptolithClient.lookup, option.VAL, staticData.skills);
  }
}

export {
  getSelectOption ,
  getSelectOptionName ,
  getSelectOptionCost ,
  getActiveOptions1 ,
  mapActiveOptions1 ,
  getActiveSelectOptions1 ,
  getActiveOptions2 ,
  getActiveOptions2Map ,
  getOption ,
  getOption1 ,
  getOption2 ,
  getOption3 ,
  getCustomInput ,
  getGenericId ,
  lookupMap ,
  getSkillFromOption ,
  
}
/* Ley_IntMap-OptolithClient Not a pure module */
