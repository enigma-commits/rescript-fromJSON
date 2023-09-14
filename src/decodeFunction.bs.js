// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function replaceFirstLetterLower(inputString) {
  if (inputString.length > 0) {
    return inputString.charAt(0).toLowerCase() + inputString.slice(1);
  } else {
    return inputString;
  }
}

function replaceFirstLetterUpper(inputString) {
  if (inputString.length > 0) {
    return inputString.charAt(0).toUpperCase() + inputString.slice(1);
  } else {
    return inputString;
  }
}

function getRecordType(str, type_) {
  var re = new RegExp("(type\\s" + type_ + "+)\\s[=]\n*\\s+({([a-zA-Z0-9_ :.,\n*\\s<>]+)})+");
  var vareintRe = new RegExp("type\\s+" + type_ + "+\\s*=\\s*(\|[^|]+)");
  var vareintTypeRe = new RegExp("type\\s+" + type_ + "\\s*=\\s*[\\|?\\s]\\s*([A-Z][a-zA-Z0-9()_\\s\|]+)\n");
  var match = str.match(re);
  if (match !== null) {
    return [
            /* Normal */1,
            Caml_array.get(Belt_Array.keepMap(match, (function (x) {
                        return x;
                      })), 2)
          ];
  }
  var match$1 = str.match(vareintRe);
  if (match$1 === null) {
    return [
            /* Normal */1,
            "// Unable to find type " + type_
          ];
  }
  var match$2 = str.match(vareintTypeRe);
  if (match$2 !== null) {
    return [
            /* Varient */0,
            Caml_array.get(Belt_Array.keepMap(match$2, (function (x) {
                        return x;
                      })), 1)
          ];
  } else {
    return [
            /* Varient */0,
            "// Unable to find type " + type_
          ];
  }
}

function defaultUserTypedValue(str) {
  if (str.startsWith("option")) {
    return "None";
  } else if (str.startsWith("array")) {
    return "[]";
  } else if (str.startsWith("Js.Nullable")) {
    return "Js.Json.null";
  } else {
    return "default" + replaceFirstLetterUpper(str) + "";
  }
}

function defaultValueMapper(str) {
  switch (str) {
    case "Js.Json.t" :
        return "Js.Dict.empty()->Js.Json.object_";
    case "bool" :
        return "false";
    case "float" :
        return "0.0";
    case "int" :
        return "0";
    case "string" :
        return "\"\"";
    default:
      return defaultUserTypedValue(str);
  }
}

function filterTypeName(str) {
  return str.replace(/array<([a-zA-Z\.<>.]+)>/g, "$1").replace(/option<([a-zA-Z\.<>.]+)>/g, "$1");
}

function getDefaultValue(str, type_) {
  var type_$1 = filterTypeName(type_);
  var match = getRecordType(str, type_$1);
  var blockScopeTypes = match[1];
  var convertedBlock = blockScopeTypes.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g, (function (param, first, second, param$1, param$2) {
          var valueMapper = defaultValueMapper(second);
          return "" + first + " : " + valueMapper + "";
        }));
  if (match[0]) {
    return "let default" + replaceFirstLetterUpper(type_$1) + " = " + convertedBlock + "\n\n";
  }
  var $$default = Belt_Array.get(blockScopeTypes.split("|"), 1);
  if ($$default !== undefined) {
    return "let default" + replaceFirstLetterUpper(type_$1) + " = " + $$default + "\n";
  } else {
    return "";
  }
}

function funcWarning(str) {
  return str.replace(/\|\s\"[A-Z][A-Za-z0-9]+[\(]+[a-zA-Z0-9_]+[\)]+\"\s=>\s([A-Z][A-Za-z0-9]+)[\(]+([a-zA-Z0-9_]+)[\)]+/g, (function (param, first, second, param$1, param$2) {
                return "| \"" + first + "\" => get" + replaceFirstLetterUpper(second) + "() // Call you custom function here";
              }));
}

function getFunctionName(str) {
  return str.replace(/(\([a-zA-Z0-9_\",\s]+\))/g, "");
}

function typeFunctionMapper(str, type_) {
  switch (str) {
    case "Js.Json.t" :
        return "getJsonObjectFromDict(dict, \"" + type_ + "\")";
    case "array<float>" :
        return "getFloatArrayFromDict(dict, \"" + type_ + "\")";
    case "array<int>" :
        return "getIntArrayFromDict(dict, \"" + type_ + "\", [])";
    case "array<string>" :
        return "getStrArrayFromDict(dict, \"" + type_ + "\", [])";
    case "bool" :
        return "getBool(dict, \"" + type_ + "\", false)";
    case "float" :
        return "getFloat(dict, \"" + type_ + "\", 0.0)";
    case "int" :
        return "getInt(dict, \"" + type_ + "\", 0)";
    case "option<Js.Json.t>" :
        return "getOptionalJsonFromDict(dict, \"" + type_ + "\")";
    case "option<array<float>>" :
        return "getOptionFloatArrayFromDict(dict, \"" + type_ + "\")";
    case "option<array<int>>" :
        return "getOptionIntArrayFromDict(dict, \"" + type_ + "\")";
    case "option<array<string>>" :
        return "getOptionStrArrayFromDict(dict, \"" + type_ + "\")";
    case "option<bool>" :
        return "getOptionBool(dict, \"" + type_ + "\")";
    case "option<float>" :
        return "getOptionFloat(dict, \"" + type_ + "\")";
    case "option<int>" :
        return "getOptionInt(dict, \"" + type_ + "\")";
    case "option<string>" :
        return "getOptionString(dict, \"" + type_ + "\")";
    case "string" :
        return "getString(dict, \"" + type_ + "\", \"\")";
    default:
      return "||get" + replaceFirstLetterUpper(type_) + "(dict, \"" + type_ + "\")**" + str + "**||";
  }
}

function getFuntionStr(fnName, type_, convertedBlock, kind) {
  var getKeyStr = "let " + fnName + " = (dict, key) => {\n  dict\n  ->Js.Dict.get(key)";
  var flatMapObjectDecodeStr = "->Belt.Option.flatMap(Js.Json.decodeObject)";
  if (/^option<([a-zA-Z0-9]+)>/.test(type_) && kind === /* Normal */1) {
    return "" + getKeyStr + "\n  " + flatMapObjectDecodeStr + "\n  ->Belt.Option.map(dict => {\n" + convertedBlock + "\n  })\n}\n";
  } else if (/^option<([a-zA-Z0-9]+)>/.test(type_) && kind === /* Varient */0) {
    return "" + getKeyStr + "\n  ->Belt.Option.flatMap(Js.Json.decodeString)\n  ->Belt.Option.map(str => {\n      \n" + convertedBlock + "\n  })\n}\n";
  } else if (/^array<([a-zA-Z0-9.]+)>/.test(type_)) {
    var optional = false;
    return "" + getKeyStr + "\n   ->Belt.Option.flatMap(Js.Json.decodeArray)\n   ->Belt.Option.getWithDefault([])\n   ->Belt.Array.keepMap(Js.Json.decodeObject)\n   ->Js.Array2.map(dict => {\n       " + convertedBlock + "\n   })" + (
            optional ? "->Some" : ""
          ) + "\n}\n";
  } else if (/^option<array<([a-zA-Z0-9]+)>>/.test(type_)) {
    return "let " + fnName + " = (dict, key) => {\n    switch dict->Js.Dict.get(key)->Belt.Option.flatMap(Js.Json.decodeArray) {\n  | Some(arr) =>\n    arr\n    " + flatMapObjectDecodeStr + "\n    ->Js.Array2.map(dict => {\n       " + convertedBlock + "\n    })\n    ->Some\n  | None => None\n   }\n}\n";
  } else if (kind === /* Normal */1) {
    return "" + getKeyStr + "\n  " + flatMapObjectDecodeStr + "\n  ->Belt.Option.map(dict => {\n     " + convertedBlock + "\n  })->Belt.Option.getWithDefault(default" + replaceFirstLetterUpper(type_) + ")\n}\n";
  } else {
    return "" + getKeyStr + "\n  ->Belt.Option.flatMap(Js.Json.decodeString)\n  ->Belt.Option.map(str => {\n      \n" + convertedBlock + "\n  })->Belt.Option.getWithDefault(default" + replaceFirstLetterUpper(type_) + ")\n}\n";
  }
}

function getObjectFunction(mainStr, fnName, type_) {
  var type_$1 = filterTypeName(type_);
  var match = getRecordType(mainStr, type_$1);
  var blockScopeTypes = match[1];
  var kind = match[0];
  var convertedBlock;
  if (kind) {
    convertedBlock = blockScopeTypes.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g, (function (param, first, second, param$1, param$2) {
            var valueMapper = typeFunctionMapper(second, first);
            return "\t" + first + " : " + valueMapper + " ";
          }));
  } else {
    var varientArr = blockScopeTypes.split("|");
    var caseBlock = funcWarning(varientArr.reduce((function (acc, item) {
                var item$1 = item.replace("\n", "");
                return acc + ("| \"" + item$1.trim() + "\" => " + item$1 + "\n");
              }), ""));
    var $$default = Belt_Option.getWithDefault(Belt_Array.get(varientArr, 0), "No Default Varient found (Please Provide one)");
    convertedBlock = "  switch str {\n      \n" + caseBlock + "\n| _ => " + $$default + " //Ensure that appropriate default values are provided.\n\n    }";
  }
  var fnName$1 = getFunctionName(fnName);
  return getFuntionStr(fnName$1, type_, convertedBlock, kind);
}

function generateNestedObject(str, mainStr) {
  var someVal = {
    contents: str
  };
  var _str = str;
  while(true) {
    var str$1 = _str;
    var match = str$1.match(/\|\|(.*?)\*\*(.*?)\*\*\|\|/);
    if (match === null) {
      return someVal.contents;
    }
    var match$1 = Belt_Array.keepMap(match, (function (x) {
            return x;
          }));
    var defaultValue = getDefaultValue(mainStr, Caml_array.get(match$1, 2));
    var nestedObject = getObjectFunction(mainStr, Caml_array.get(match$1, 1), Caml_array.get(match$1, 2));
    someVal.contents = (defaultValue + "\n" + nestedObject + "\n" + someVal.contents).replace(Caml_array.get(match$1, 0), Caml_array.get(match$1, 1));
    _str = someVal.contents;
    continue ;
  };
}

function generateDecode(type_, mainStr) {
  var match = getRecordType(mainStr, type_);
  var blockScopeTypes = match[1];
  var kind = match[0];
  var convertedBlock;
  if (kind) {
    convertedBlock = blockScopeTypes.replace(/(@as+[A-Za-z0-9_()^@\\\\\"]+)?\s+([A-Za-z0-9_\"\\^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g, (function (param, first, second, third, param$1, param$2) {
            var first$1 = Belt_Option.getWithDefault(first, "").replace("\"", "").replace("\"", "").replace("(", "").replace(")", "").replace("@as", "");
            var second$1 = second.trim();
            var third$1 = third.trim();
            var valueMapper = typeFunctionMapper(third$1, first$1.length > 0 ? first$1 : second$1);
            return "\t" + second$1 + " : " + valueMapper + "\n";
          }));
  } else {
    var caseBlock = funcWarning(blockScopeTypes.split("|").reduce((function (acc, item) {
                var item$1 = item.replace("\n", "");
                return acc + ("| \"" + item$1.trim() + "\" => " + item$1 + "\n");
              }), ""));
    convertedBlock = "  switch str {\n      \n" + caseBlock + "\n    }";
  }
  var mapper = kind ? "let itemToObjectMapper = dict => {\n" + convertedBlock + "\n}\n " : "let get" + replaceFirstLetterUpper(type_) + " = str => {\n" + convertedBlock + "\n}\n ";
  return generateNestedObject(mapper, mainStr);
}

exports.replaceFirstLetterLower = replaceFirstLetterLower;
exports.replaceFirstLetterUpper = replaceFirstLetterUpper;
exports.getRecordType = getRecordType;
exports.defaultUserTypedValue = defaultUserTypedValue;
exports.defaultValueMapper = defaultValueMapper;
exports.filterTypeName = filterTypeName;
exports.getDefaultValue = getDefaultValue;
exports.funcWarning = funcWarning;
exports.getFunctionName = getFunctionName;
exports.typeFunctionMapper = typeFunctionMapper;
exports.getFuntionStr = getFuntionStr;
exports.getObjectFunction = getObjectFunction;
exports.generateNestedObject = generateNestedObject;
exports.generateDecode = generateDecode;
/* No side effect */
