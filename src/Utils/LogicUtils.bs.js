// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_dict = require("rescript/lib/js/js_dict.js");
var Js_json = require("rescript/lib/js/js_json.js");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Float = require("rescript/lib/js/belt_Float.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");

function getOptionString(dict, key) {
  return Belt_Option.flatMap(Js_dict.get(dict, key), Js_json.decodeString);
}

function getString(dict, key, $$default) {
  return Belt_Option.getWithDefault(getOptionString(dict, key), $$default);
}

function getIntFromString(str, $$default) {
  var $$int = Belt_Int.fromString(str);
  if ($$int !== undefined) {
    return $$int;
  } else {
    return $$default;
  }
}

function getIntFromJson(json, $$default) {
  var str = Js_json.classify(json);
  if (typeof str === "number") {
    return $$default;
  }
  switch (str.TAG | 0) {
    case /* JSONString */0 :
        return getIntFromString(str._0, $$default);
    case /* JSONNumber */1 :
        return str._0 | 0;
    default:
      return $$default;
  }
}

function getInt(dict, key, $$default) {
  var value = Js_dict.get(dict, key);
  if (value !== undefined) {
    return getIntFromJson(Caml_option.valFromOption(value), $$default);
  } else {
    return $$default;
  }
}

function getFloatFromString(str, $$default) {
  var floatVal = Belt_Float.fromString(str);
  if (floatVal !== undefined) {
    return floatVal;
  } else {
    return $$default;
  }
}

var getOptionFloatFromString = Belt_Float.fromString;

function getOptionFloatFromJson(json) {
  var str = Js_json.classify(json);
  if (typeof str === "number") {
    return ;
  }
  switch (str.TAG | 0) {
    case /* JSONString */0 :
        return Belt_Float.fromString(str._0);
    case /* JSONNumber */1 :
        return str._0;
    default:
      return ;
  }
}

function getFloatFromJson(json, $$default) {
  var str = Js_json.classify(json);
  if (typeof str === "number") {
    return $$default;
  }
  switch (str.TAG | 0) {
    case /* JSONString */0 :
        return getFloatFromString(str._0, $$default);
    case /* JSONNumber */1 :
        return str._0;
    default:
      return $$default;
  }
}

function getOptionFloat(dict, key) {
  var value = Js_dict.get(dict, key);
  if (value !== undefined) {
    return getOptionFloatFromJson(Caml_option.valFromOption(value));
  }
  
}

function getFloat(dict, key, $$default) {
  return Belt_Option.getWithDefault(Belt_Option.map(Js_dict.get(dict, key), (function (json) {
                    return getFloatFromJson(json, $$default);
                  })), $$default);
}

function getOptionBool(dict, key) {
  return Belt_Option.flatMap(Js_dict.get(dict, key), Js_json.decodeBoolean);
}

function getBool(dict, key, $$default) {
  return Belt_Option.getWithDefault(getOptionBool(dict, key), $$default);
}

var getOptionIntFromString = Belt_Int.fromString;

function getOptionIntFromJson(json) {
  var str = Js_json.classify(json);
  if (typeof str === "number") {
    return ;
  }
  switch (str.TAG | 0) {
    case /* JSONString */0 :
        return Belt_Int.fromString(str._0);
    case /* JSONNumber */1 :
        return str._0 | 0;
    default:
      return ;
  }
}

function getOptionInt(dict, key) {
  var value = Js_dict.get(dict, key);
  if (value !== undefined) {
    return getOptionIntFromJson(Caml_option.valFromOption(value));
  }
  
}

function getOptionBool$1(dict, key) {
  return Belt_Option.flatMap(Js_dict.get(dict, key), Js_json.decodeBoolean);
}

var getOptionalJsonFromDict = Js_dict.get;

function getJsonObjectFromDict(dict, key) {
  return Belt_Option.getWithDefault(Js_dict.get(dict, key), {});
}

function getStrArrayFromJsonArray(jsonArr) {
  return Belt_Array.keepMap(jsonArr, Js_json.decodeString);
}

function getOptionStrArrayFromJson(json) {
  return Belt_Option.map(Js_json.decodeArray(json), getStrArrayFromJsonArray);
}

function getStrArrayFromDict(dict, key, $$default) {
  return Belt_Option.getWithDefault(Belt_Option.flatMap(Js_dict.get(dict, key), getOptionStrArrayFromJson), $$default);
}

function getFloatArrayFromDict(dict, key) {
  return Belt_Option.getWithDefault(Belt_Option.flatMap(Js_dict.get(dict, key), (function (json) {
                    return Belt_Option.map(Js_json.decodeArray(json), (function (arr) {
                                  return Belt_Array.keepMap(arr, Js_json.decodeNumber);
                                }));
                  })), []);
}

function getIntArrayFromJsonArray(jsonArr) {
  return Belt_Array.keepMap(jsonArr, Js_json.decodeNumber).map(function (prim) {
              return prim | 0;
            });
}

function getOptionIntArrayFromJson(json) {
  return Belt_Option.map(Js_json.decodeArray(json), getIntArrayFromJsonArray);
}

function getIntArrayFromDict(dict, key, $$default) {
  return Belt_Option.getWithDefault(Belt_Option.flatMap(Js_dict.get(dict, key), getOptionIntArrayFromJson), $$default);
}

function getOptionFloatArrayFromDict(dict, key) {
  return Belt_Option.flatMap(Js_dict.get(dict, key), (function (json) {
                return Belt_Option.map(Js_json.decodeArray(json), (function (arr) {
                              return Belt_Array.keepMap(arr, Js_json.decodeNumber);
                            }));
              }));
}

function getOptionStrArrayFromDict(dict, key) {
  return Belt_Option.flatMap(Js_dict.get(dict, key), getOptionStrArrayFromJson);
}

function getOptionIntArrayFromDict(dict, key) {
  return Belt_Option.flatMap(Js_dict.get(dict, key), getOptionIntArrayFromJson);
}

exports.getOptionString = getOptionString;
exports.getString = getString;
exports.getIntFromString = getIntFromString;
exports.getIntFromJson = getIntFromJson;
exports.getInt = getInt;
exports.getFloatFromString = getFloatFromString;
exports.getOptionFloatFromString = getOptionFloatFromString;
exports.getOptionFloatFromJson = getOptionFloatFromJson;
exports.getFloatFromJson = getFloatFromJson;
exports.getOptionFloat = getOptionFloat;
exports.getFloat = getFloat;
exports.getBool = getBool;
exports.getOptionIntFromString = getOptionIntFromString;
exports.getOptionIntFromJson = getOptionIntFromJson;
exports.getOptionInt = getOptionInt;
exports.getOptionBool = getOptionBool$1;
exports.getOptionalJsonFromDict = getOptionalJsonFromDict;
exports.getJsonObjectFromDict = getJsonObjectFromDict;
exports.getStrArrayFromJsonArray = getStrArrayFromJsonArray;
exports.getOptionStrArrayFromJson = getOptionStrArrayFromJson;
exports.getStrArrayFromDict = getStrArrayFromDict;
exports.getFloatArrayFromDict = getFloatArrayFromDict;
exports.getIntArrayFromJsonArray = getIntArrayFromJsonArray;
exports.getOptionIntArrayFromJson = getOptionIntArrayFromJson;
exports.getIntArrayFromDict = getIntArrayFromDict;
exports.getOptionFloatArrayFromDict = getOptionFloatArrayFromDict;
exports.getOptionStrArrayFromDict = getOptionStrArrayFromDict;
exports.getOptionIntArrayFromDict = getOptionIntArrayFromDict;
/* No side effect */
