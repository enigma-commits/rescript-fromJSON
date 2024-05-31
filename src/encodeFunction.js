var Caml_array = require("rescript/lib/js/caml_array.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Js_dict = require("rescript/lib/js/js_dict.js");


function replaceFirstLetterLower(inputString) {
  if (inputString.length > 0) {
    return inputString.charAt(0).toLowerCase() + inputString.slice(1);
  } else {
    return inputString;
  }
}

function replaceFirstLetterUpper(inputString) {
  if (inputString.length > 0) {
    inputString = inputString.split('.');
    let result = "";
    for(let i=0;i<inputString.length;i++){
      result += inputString[i].charAt(0).toUpperCase() + inputString[i].slice(1);
    }
    return result;
  } else {
    return inputString;
  }
}
///(type\s*quoteAPIContents*+)\s[=] *\s+({([a-zA-Z0-9_ :., *\s<>]+)})+/: Nothing to repeat
function getRecordType(_str, _type_, allfiles) {
    while(true) {
      var type_ = _type_;
      var str = _str;
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
      if (match$1 !== null) {
        var match$2 = str.match(vareintTypeRe);
        if (match$2 !== null) {
          return [
                  /* Varient */0,
                  Caml_array.get(Belt_Array.keepMap(match$2, (function (x) {
                              return x;
                            })), 1)
                ];
        }
        var match$3 = type_.split(".");
        if (match$3.length !== 2) {
          return [
                  /* Varient */0,
                  "// Unable to find type of " + type_
                ];
        }
        var module_ = match$3[0];
        var type1_ = match$3[1];
        var file = Js_dict.get(allfiles, module_);
        if (file === undefined) {
          return [
                  /* Varient */0,
                  "// Unable to find type of " + type1_
                ];
        }
        _type_ = type1_;
        _str = file;
        continue ;
      }
      var match$4 = type_.split(".");
      if (match$4.length !== 2) {
        return [
                /* Normal */1,
                "// Unable to find type of " + type_
              ];
      }
      var module_$1 = match$4[0];
      var type1_$1 = match$4[1];
      var file$1 = Js_dict.get(allfiles, module_$1);
      if (file$1 === undefined) {
        return [
                /* Normal */1,
                "// Unable to find type of " + type1_$1
              ];
      }
      _type_ = type1_$1;
      _str = file$1;
      continue ;
    };
  }

  function typeFunctionMapper(str, type_) {
    switch (str) {
      case "Js.Json.t" :
          return "encodeJsonObject";
      case "array<float>" :
          return "encodeFloatArray";
      case "array<int>" :
          return "encodeIntArray";
      case "array<string>" :
          return "encodeStrArray";
      case "bool" :
          return "encodeBool";
      case "float" :
          return "encodeFloat";
      case "int" :
          return "encodeInt";
      case "option<Js.Json.t>" :
          return "encodeOptionalJson";
      case "option<array<float>>" :
          return "encodeOptionFloatArray";
      case "option<array<int>>" :
          return "encodeOptionIntArray";
      case "option<array<string>>" :
          return "encodeOptionStrArray";
      case "option<bool>" :
          return "encodeOptionBool";
      case "option<float>" :
          return "encodeOptionFloat";
      case "option<int>" :
          return "encodeOptionInt";
      case "option<string>" :
          return "encodeOptionString";
      case "string" :
          return "encodeString";
      default:
        const optionTypePattern = /option<\s*([^>]+)\s*>/;
        const match = str.match(optionTypePattern);
        if (match) {
          const arrayTypePattern = /array<\s*([^>]+)\s*>/;
          const match1 = str.match(arrayTypePattern);
          if (match1) 
            return "||Belt.Option.mapWithDefault(Js.Json.null, (array => array -> Array.map(encode" + replaceFirstLetterUpper(match1[1].trim()) + ")->JSON.Encode.array))**" + match1[1].trim() + "**||"
          return "||encode" + replaceFirstLetterUpper(match[1].trim()+"Option") + "***" + match[1].trim() + "***||"
        }
        const arrayTypePattern = /array<\s*([^>]+)\s*>/;
        const match1 = str.match(arrayTypePattern);
        if (match1) {
          return "||Array.map(encode" + replaceFirstLetterUpper(match1[1].trim()) + ") -> JSON.Encode.array**" + match1[1].trim() + "**||"
        }
        return "||encode" + replaceFirstLetterUpper(str) + "**" + str + "**||";
    }
  }

function extractInnerContent(str) {
  const regex = /^.+\((.+)\)$/;
  let match = regex.exec(str);

  while (match && match[1].includes('(')) {
    match = regex.exec(match[1]);
  }

  return match ? match[1] : null;
}

function generateEncodeHelperOption(blockScopedType, typeName) {
  var objectRegEx = new RegExp("\\{\\s*[^}]*\\s*\\}", "g");
  if(objectRegEx.test(blockScopedType)){
    let prefix = "\tif req == None {\n\t\tJs.Json.null\n\t} else {\n\t\tlet req=req -> Option.getExn\n\t\t";
    let result = blockScopedType.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g, (function (param, first, second, param$1, param$2) {
      var valueMapper = typeFunctionMapper(second, first);
      return "\t\t" + `("${first}"` + ` , req.${first} -> ` + valueMapper + ")"}));
    result = result.replace(/{/g, '([');
    result = result.replace(/}/g, '])');
    return (prefix + "Js.Dict.fromArray"+result + " -> Js.Json.object_")+"\n\t}";
  } else {
    let result = "let encode" + replaceFirstLetterUpper(typeName) + "Option" + " = req =>\n";
    result += `\tswitch(req) {\n`;
    let newBlockScopedType = blockScopedType.replace(/[\n\s]/g, '');
    let types = newBlockScopedType.split("|");
    for(let i = 0;i<types.length;i++){
      let val1 = extractInnerContent(types[i]);
      const optionTypePattern = /option<\s*([^>]+)\s*>/;
        const match = types[i].match(optionTypePattern);
        if (match) {
          const arrayTypePattern = /array<\s*([^>]+)\s*>/;
          const match1 = types[i].match(arrayTypePattern);
          if (match1) {
            return "||Belt.Option.mapWithDefault(Js.Json.null, (array => array -> Array.map(encode" + replaceFirstLetterUpper(match1[1].trim()) + ") -> JSON.Encode.array))**" + match1[1].trim() + "**||"
          }
          return "||encode" + replaceFirstLetterUpper(match[1].trim()+"Option") + "***" + match[1].trim() + "***||"
        } else {
          const arrayTypePattern = /array<\s*([^>]+)\s*>/;
          const match1 = types[i].match(arrayTypePattern);
          if (match1) {
            return `${types[i]} => ||Array.map(encode${replaceFirstLetterUpper(match1[1])}(${match1[1]})" + ") -> JSON.Encode.array**" + ${match1[1]} + "**||`
          }
          if(val1==null) {
            result += `\t| Some(${types[i]}) => "${types[i]}" -> JSON.Encode.string\n`
          } else {
            result += `\t| Some(${types[i]}) => ||encode${replaceFirstLetterUpper(val1)}(${val1})**${val1}**||\n`
          }
        }
    }
    result += `\t| _ => Js.Json.null}\n`;
    return result;
  }
}

function generateEncodeHelper(blockScopedType, typeName) {
  var objectRegEx = new RegExp("\\{\\s*[^}]*\\s*\\}", "g");
  if(objectRegEx.test(blockScopedType)){
    let result = (blockScopedType.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g, (function (param, first, second, param$1, param$2) {
      var valueMapper = typeFunctionMapper(second, first);
      return "\t\t" + `("${first}"` + ` , req.${first} -> ` + valueMapper + ")";
    })) + " -> Js.Json.object_\n\n");
    result = result.replace(/{/g, '([');
    result = result.replace(/}/g, '])');
    return "Js.Dict.fromArray"+result;
  } else {
    let result = "let encode" + replaceFirstLetterUpper(typeName) + " = req =>\n";
    result += `\tswitch(req) {\n`;
    let newBlockScopedType = blockScopedType.replace(/[\n\s]/g, '');
    let types = newBlockScopedType.split("|");
    for(let i = 0;i<types.length;i++){
      let val1 = extractInnerContent(types[i]);
      const optionTypePattern = /option<\s*([^>]+)\s*>/;
      const match = types[i].match(optionTypePattern);
      if (match) {
        const arrayTypePattern = /array<\s*([^>]+)\s*>/;
        const match1 = types[i].match(arrayTypePattern);
        if (match1)
          // Belt.Option.mapWithDefault(Js.Json.null, (array => array -> Array.map(encodeEstimateFaresOption)->JSON.Encode.array)),
          return "Belt.Option.mapWithDefault(Js.Json.null, (array => array -> Array.map(encode" + replaceFirstLetterUpper(match1[1].trim()) + ") -> JSON.Encode.array))**" + match1[1].trim() + "**||"
        return "||encode" + replaceFirstLetterUpper(match[1].trim()+"Option") + "***" + match[1].trim() + "***||"
      } else {
        const arrayTypePattern = /array<\s*([^>]+)\s*>/;
        const match1 = types[i].match(arrayTypePattern);
        if (match1) {
          return `${types[i]} => ||Array.map(encode${replaceFirstLetterUpper(match1[1])}(${match1[1]})" + ") -> JSON.Encode.array**" + ${match1[1]} + "**||`
        }
        if(val1==null) {
          result += `\t| ${types[i]} => "${types[i]}" -> JSON.Encode.string\n`
        } else {
          result += `\t| ${types[i]} => ||encode${replaceFirstLetterUpper(val1)}(${val1})**${val1}**||\n`
        }
      }
    }
    result += `\t}\n`;
    return result;
  }
}

function recursiveGenerate(contents, fileContents, files, hashMap) {
  var match = contents.match(/\|\|(.*?)\*\*\*(.*?)\*\*\*\|\|/);
  if(match === null) {
    var match = contents.match(/\|\|(.*?)\*\*(.*?)\*\*\|\|/);
    if(match === null) {
      return contents;
    }
    let match1 = getRecordType(fileContents, match[2], files);
    let blockScopedType = match1[1];
    let kind = match1[0];
    contents = contents.replace(match[0], match[1]);
    if(match[2] in hashMap)
      return recursiveGenerate(contents, fileContents, files, hashMap);
    hashMap[match[2]] = true;
    if(kind==0){
      return recursiveGenerate(generateEncodeHelper(blockScopedType, match[2]) + contents, fileContents, files, hashMap);
    } else {
      var prefix = `let encode${replaceFirstLetterUpper(match[2])} = req =>\n`;
      return recursiveGenerate(prefix + generateEncodeHelper(blockScopedType, match[2]) +'\n' + contents, fileContents, files, hashMap);
    }
  }
  let match1 = getRecordType(fileContents, match[2], files);
    let blockScopedType = match1[1];
    let kind = match1[0];
    contents = contents.replace(match[0], match[1]);
    if(match[2]+"Option" in hashMap)
      return recursiveGenerate(contents, fileContents, files, hashMap);
    hashMap[match[2]+"Option"] = true;
    if(kind==0){
      return recursiveGenerate(generateEncodeHelperOption(blockScopedType, match[2]) + contents, fileContents, files, hashMap);
    } else {
      var prefix = `let encode${replaceFirstLetterUpper(match[2])}Option = req =>\n`;
      return recursiveGenerate(prefix + generateEncodeHelperOption(blockScopedType, match[2]) +'\n' + contents, fileContents, files, hashMap);
    }
}

function generateEncode(typeName,fileContents, files) {
  var match = getRecordType(fileContents, typeName, files);
  var blockScopedType = match[1];
  var kind = match[0];
  var prefix = `let encode${replaceFirstLetterUpper(typeName)} = req =>\n`;
  let result = ""
  if(kind==0) {
    result = generateEncodeHelper(blockScopedType, typeName) + prefix + `Js.Json.object_(Js.Dict.fromArray([("value", encode${replaceFirstLetterUpper(typeName)}(req))]))`;
  } else {
    result = prefix + generateEncodeHelper(blockScopedType,typeName);
  }
  let hashMap = {}
  return recursiveGenerate(result, fileContents, files, hashMap);
}

exports.generateEncode = generateEncode;