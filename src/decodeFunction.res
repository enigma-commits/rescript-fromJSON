let replaceFirstLetterLower = (inputString: string) => {
  if inputString->Js.String2.length > 0 {
    inputString->Js.String2.charAt(0)->Js.String2.toLowerCase ++
      inputString->Js.String2.sliceToEnd(~from=1)
  } else {
    inputString
  }
}

let replaceFirstLetterUpper = (inputString: string) => {
  if inputString->Js.String2.length > 0 {
    inputString->Js.String2.charAt(0)->Js.String2.toUpperCase ++
      inputString->Js.String2.sliceToEnd(~from=1)
  } else {
    inputString
  }
}

type type_ = Varient | Normal

let rec getRecordType = (str, type_, allfiles) => {
  let re = `(type\\s${type_}+)\\s[=]\n*\\s+({([a-zA-Z0-9_ :.,\n*\\s<>]+)})+`->Js.Re.fromString
  let vareintRe = `type\\s+${type_}+\\s*=\\s*(\|[^|]+)`->Js.Re.fromString
  let vareintTypeRe =
    `type\\s+${type_}\\s*=\\s*[\\|?\\s]\\s*([A-Z][a-zA-Z0-9()_\\s\|]+)\n`->Js.Re.fromString

  // let vereintTypeRe2 = `type\\s+${type_}\\s*=\\s*(\\|?\\s*[A-Z][A-Z0-9_]*\\([a-zA-Z0-9_]+\\)(\\s*\\|\\s*[A-Z][A-Z0-9_]*\\([a-zA-Z0-9_]+\\))*)\n` -> Js.Re.fromString

  switch str->Js.String2.match_(re) {
  | Some(match) => (Normal, (match->Belt.Array.keepMap(x => x))[2])
  | None =>
    switch str->Js.String2.match_(vareintRe) {
    | Some(_) =>
      switch str->Js.String2.match_(vareintTypeRe) {
      | Some(match) => (Varient, (match->Belt.Array.keepMap(x => x))[1])
      | None => switch (type_ -> Js.String2.split(".")) {
          | [module_, type1_] =>
            switch allfiles->Js.Dict.get(module_) {
              | Some(file) => {
                getRecordType(file, type1_, allfiles)
              }
              | None => (Varient, "// Unable to find type of " ++ type1_)
            }
          | _ => (Varient, "// Unable to find type of " ++ type_)
        }
      }
    | None => switch (type_ -> Js.String2.split(".")) {
          | [module_, type1_] =>
            switch allfiles->Js.Dict.get(module_) {
              | Some(file) => {
                getRecordType(file, type1_, allfiles)
              }
              | None => (Normal, "// Unable to find type of " ++ type1_)
            }
          | _ => (Normal, "// Unable to find type of " ++ type_)
        }
    }
  }
}

let defaultUserTypedValue = str => {
  if str->Js.String2.startsWith("option") {
    "None"
  } else if str->Js.String2.startsWith("array") {
    "[]"
  } else if str->Js.String2.startsWith("Js.Nullable") {
    "Js.Json.null"
  } else {
    `default${str->replaceFirstLetterUpper}`
  }
}

let defaultValueMapper = str => {
  switch str {
  | "string" => `""`
  | "int" => `0`
  | "bool" => `false`
  | "float" => `0.0`
  | "Js.Json.t" => "Js.Dict.empty()->Js.Json.object_"
  | str => str->defaultUserTypedValue
  }
}
let filterTypeName = str => {
  str
  ->Js.String2.replaceByRe(%re("/array<([a-zA-Z\.<>.]+)>/g"), "$1")
  ->Js.String2.replaceByRe(%re("/option<([a-zA-Z\.<>.]+)>/g"), "$1")
}
let getDefaultValue = (str, type_, allfiles) => {
  let type_ = type_->filterTypeName

  let (kind, blockScopeTypes) = str->getRecordType(type_, allfiles)

  let convertedBlock = blockScopeTypes->Js.String2.unsafeReplaceBy2(
    %re("/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g"),
    (_, first, second, _, _) => {
      let valueMapper = second->defaultValueMapper

      `${first} : ${valueMapper}`
    },
  )

  switch kind {
  | Varient => {
      let default = blockScopeTypes->Js.String2.split("|")->Belt.Array.get(1)
      switch default {
      | Some(val) => 
        let regex = %re("/^([a-zA-Z_]\w*)\(([a-zA-Z_]\w*)\)$/")
        switch Js.Re.exec_( regex, Js.String2.trim(val)) {
          | Some(match') =>
            let match = Js.Re.matches(match')
            let enumName = match[1]
            let enumType = match[2]
            `let default${type_->replaceFirstLetterUpper} = ${enumName}(default${enumType->replaceFirstLetterUpper})\n`
          | None => `let default${type_->replaceFirstLetterUpper} = ${val}\n`
        }
      | None => ""
    }
  }
  | Normal =>
    `let default${type_->replaceFirstLetterUpper} = ${convertedBlock}

`
  }
}

let funcWarning = str => {
  str->Js.String2.unsafeReplaceBy2(
    %re(
      "/\|\s\"[A-Z][A-Za-z0-9]+[\(]+[a-zA-Z0-9_]+[\)]+\"\s=>\s([A-Z][A-Za-z0-9]+)[\(]+([a-zA-Z0-9_]+)[\)]+/g"
    ),
    (_, first, second, _, _) => {
      `| "${first}" => get${second->replaceFirstLetterUpper}() // Call you custom function here`
    },
  )
}

let getFunctionName = str => {
  str->Js.String2.replaceByRe(%re("/(\([a-zA-Z0-9_\",\s]+\))/g"), "")
}

let typeFunctionMapper = (str, type_) => {
  switch str {
  | "string" => `getString(dict, "${type_}", "")`
  | "int" => `getInt(dict, "${type_}", 0)`
  | "float" => `getFloat(dict, "${type_}", 0.0)`
  | "bool" => `getBool(dict, "${type_}", false)`
  | "option<float>" => `getOptionFloat(dict, "${type_}")`
  | "option<string>" => `getOptionString(dict, "${type_}")`
  | "option<int>" => `getOptionInt(dict, "${type_}")`
  | "option<bool>" => `getOptionBool(dict, "${type_}")`
  | "array<float>" => `getFloatArrayFromDict(dict, "${type_}")`
  | "option<Js.Json.t>" => `getOptionalJsonFromDict(dict, "${type_}")`
  | "Js.Json.t" => `getJsonObjectFromDict(dict, "${type_}")`
  | "array<string>" => `getStrArrayFromDict(dict, "${type_}", [])`
  | "array<int>" => `getIntArrayFromDict(dict, "${type_}", [])`
  | "option<array<float>>" => `getOptionFloatArrayFromDict(dict, "${type_}")`
  | "option<array<string>>" => `getOptionStrArrayFromDict(dict, "${type_}")`
  | "option<array<int>>" => `getOptionIntArrayFromDict(dict, "${type_}")`
  | str => `||get${type_->replaceFirstLetterUpper}(dict, "${type_}")**${str}**||`
  }
}

let typeFunctionMapperEnum = (str, type_) => {
  switch str {
  | "string" => `getString(dict, "${type_}", "")`
  | "int" => `getInt(dict, "${type_}", 0)`
  | "float" => `getFloat(dict, "${type_}", 0.0)`
  | "bool" => `getBool(dict, "${type_}", false)`
  | "option<float>" => `getOptionFloat(dict, "${type_}")`
  | "option<string>" => `getOptionString(dict, "${type_}")`
  | "option<int>" => `getOptionInt(dict, "${type_}")`
  | "option<bool>" => `getOptionBool(dict, "${type_}")`
  | "array<float>" => `getFloatArrayFromDict(dict, "${type_}")`
  | "option<Js.Json.t>" => `getOptionalJsonFromDict(dict, "${type_}")`
  | "Js.Json.t" => `getJsonObjectFromDict(dict, "${type_}")`
  | "array<string>" => `getStrArrayFromDict(dict, "${type_}", [])`
  | "array<int>" => `getIntArrayFromDict(dict, "${type_}", [])`
  | "option<array<float>>" => `getOptionFloatArrayFromDict(dict, "${type_}")`
  | "option<array<string>>" => `getOptionStrArrayFromDict(dict, "${type_}")`
  | "option<array<int>>" => `getOptionIntArrayFromDict(dict, "${type_}")`
  | str => `||get${type_->replaceFirstLetterUpper}Option(dict, "${type_}")***${str}***||`
  }
}

let enumFunctionMapper = (str) => {
  Js.Console.log(`enumFunctionMapper ${str}`)
  let regex = %re("/^([a-zA-Z_]\w*)\(([a-zA-Z_]\w*)\)$/")
  switch Js.Re.exec_( regex, str) {
  | Some(match') =>
    Js.Console.log(`match`)
    let match = Js.Re.matches(match')
    let enumName = match[1]
    let enumType = match[2]
    let enumDecode = enumType -> typeFunctionMapperEnum(enumType)
    `${enumDecode} -> Belt.Option.map(x => ${enumName}(x))`
  | None => 
    let caseBlock = `\t\t| "${Js.String2.trim(str)}" => Some(${str})`
      `  switch str {
${caseBlock}
\t\t| _ => None
    }`
  }
}
let getFuntionStr = (fnName, type_, convertedBlock, kind) => {
  let getKeyStr = `let ${fnName} = (dict, key) => {
  dict
  ->Js.Dict.get(key)`
  let flatMapObjectDecodeStr = `->Belt.Option.flatMap(Js.Json.decodeObject)`

  let arrayOfObjuectStr = (~optional) => {
    `${getKeyStr}
   ->Belt.Option.flatMap(Js.Json.decodeArray)
   ->Belt.Option.getWithDefault([])
   ->Belt.Array.keepMap(Js.Json.decodeObject)
   ->Js.Array2.map(dict => {
       ${convertedBlock}
   })${optional ? "->Some" : ""}
}\n`
  }

  if Js.Re.test_(%re("/^option<([a-zA-Z0-9]+)>/"), type_) && kind == Normal {
    `${getKeyStr}
  ${flatMapObjectDecodeStr}
  ->Belt.Option.map(dict => {
${convertedBlock}
  })
}\n`
  } else if Js.Re.test_(%re("/^option<([a-zA-Z0-9]+)>/"), type_) && kind == Varient {
    `${getKeyStr}
  ->Belt.Option.flatMap(Js.Json.decodeString)
  ->Belt.Option.map(str => {
      \n${convertedBlock}
  })
}\n`
  } else if Js.Re.test_(%re("/^array<([a-zA-Z0-9.]+)>/"), type_) {
    arrayOfObjuectStr(~optional=false)
  } else if Js.Re.test_(%re("/^option<array<([a-zA-Z0-9]+)>>/"), type_) {
    `let ${fnName} = (dict, key) => {
    switch dict->Js.Dict.get(key)->Belt.Option.flatMap(Js.Json.decodeArray) {
  | Some(arr) =>
    arr
    ${flatMapObjectDecodeStr}
    ->Js.Array2.map(dict => {
       ${convertedBlock}
    })
    ->Some
  | None => None
   }
}\n`
  } else if kind === Normal {
    `${getKeyStr}
  ${flatMapObjectDecodeStr}
  ->Belt.Option.map(dict => {
     ${convertedBlock}
  })->Belt.Option.getWithDefault(default${type_->replaceFirstLetterUpper})
}\n`
  } else {
    `${getKeyStr}
  ->Belt.Option.flatMap(Js.Json.decodeString)
  ->Belt.Option.map(str => {
      \n${convertedBlock}
  })->Belt.Option.getWithDefault(default${type_->replaceFirstLetterUpper})
}\n`
  }
}

let getFuntionStrEnum = (fnName, type_, convertedBlock, kind) => {
  let getKeyStr = `let ${fnName} = (dict, _key) => {
  Some(dict)`
  let flatMapObjectDecodeStr = `->Belt.Option.flatMap(Js.Json.decodeObject)`

  let arrayOfObjuectStr = (~optional) => {
    `${getKeyStr}
   ->Belt.Option.getWithDefault([])
   ->Belt.Array.keepMap(Js.Json.decodeObject)
   ->Js.Array2.map(dict => {
      ${convertedBlock}
   })${optional ? "->Some" : ""}
}\n`
  }

  if Js.Re.test_(%re("/^option<([a-zA-Z0-9]+)>/"), type_) && kind == Normal {
    `${getKeyStr}
  ->Belt.Option.map(dict => {
${convertedBlock}
  })
}\n`
  } else if Js.Re.test_(%re("/^option<([a-zA-Z0-9]+)>/"), type_) && kind == Varient {
    `${getKeyStr}
  ->Belt.Option.map(dict => {
      \n${convertedBlock}
  })
}\n`
  } else if Js.Re.test_(%re("/^array<([a-zA-Z0-9.]+)>/"), type_) {
    arrayOfObjuectStr(~optional=false)
  } else if Js.Re.test_(%re("/^option<array<([a-zA-Z0-9]+)>>/"), type_) {
    `let ${fnName} = (dict, key) => {
    switch dict->Js.Dict.get(key)->Belt.Option.flatMap(Js.Json.decodeArray) {
  | Some(arr) =>
    arr
    ${flatMapObjectDecodeStr}
    ->Js.Array2.map(dict => {
       ${convertedBlock}
    })
    ->Some
  | None => None
   }
}\n`
  } else if kind === Normal {
    `${getKeyStr}
  ->Belt.Option.map(dict => {
     ${convertedBlock}
  })->Belt.Option.getWithDefault(default${type_->replaceFirstLetterUpper}) -> returnNonDefault(default${type_->replaceFirstLetterUpper})
}\n`
  } else {
    `${getKeyStr}
  ->Belt.Option.map(dict => {
      \n${convertedBlock}
  })->Belt.Option.getWithDefault(default${type_->replaceFirstLetterUpper}) -> returnNonDefault(default${type_->replaceFirstLetterUpper})
}\n`
  }
}

let getObjectFunction = (mainStr, fnName, type_, allFiles) => {
  let crypticType = type_
  let type_ = type_->filterTypeName
  let fnName = fnName->getFunctionName
  let (kind, blockScopeTypes) = mainStr->getRecordType(type_, allFiles)

  let convertedBlock = switch kind {
  | Varient => {
      let caseBlock = blockScopeTypes->Js.String2.split("|")->Js.Array2.reduce((acc, item) => {
          let item = item->Js.String2.replace("\n", "") -> Js.String2.trim
          acc ++ (` -> Belt.Option.orElse(${enumFunctionMapper(item)})`)
        }, "") ++ `-> Belt.Option.getExn`
      `  None
      \n${caseBlock}`
    }
  | Normal =>
    blockScopeTypes->Js.String2.unsafeReplaceBy2(
      %re("/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g"),
      (_, first, second, _, _) => {
        let valueMapper = second->typeFunctionMapper(first)

        `\t${first} : ${valueMapper} `
      },
    )
  }
  let functionStr = fnName->getFuntionStr(crypticType, convertedBlock, kind)

  functionStr
}

let getObjectFunctionEnum = (mainStr, fnName, type_, allFiles) => {
  let crypticType = type_
  let type_ = type_->filterTypeName
  let (kind, blockScopeTypes) = mainStr->getRecordType(type_, allFiles)

  let convertedBlock = switch kind {
  | Varient => {
      let caseBlock = blockScopeTypes->Js.String2.split("|")->Js.Array2.reduce((acc, item) => {
          let item = item->Js.String2.replace("\n", "") -> Js.String2.trim
          acc ++ (` -> Belt.Option.orElse(${enumFunctionMapper(item)})`)
        }, "")++` -> Belt.Option.getExn`
      `  None
      \n${caseBlock}`
    }
  | Normal =>
    blockScopeTypes->Js.String2.unsafeReplaceBy2(
      %re("/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g"),
      (_, first, second, _, _) => {
        let valueMapper = second->typeFunctionMapperEnum(first)

        `\t${first} : ${valueMapper} `
      },
    )
  }
  let fnName = fnName->getFunctionName
  let functionStr = fnName->getFuntionStrEnum(crypticType, convertedBlock, kind)

  functionStr
}

let generateNestedObject = (str, mainStr, allfiles) => {
  let someVal = ref(str)
  let rec fn = str => {
    switch str->Js.String2.match_(%re("/\|\|(.*?)\*\*\*(.*?)\*\*\*\|\|/")) {
    | Some(match) =>
      let match = match->Belt.Array.keepMap(x => x)
      let defaultValue = mainStr->getDefaultValue(match[2], allfiles)
      let nestedObject = mainStr->getObjectFunctionEnum(match[1], match[2], allfiles)

      someVal :=
          (defaultValue ++ "\n" ++ nestedObject ++ "\n" ++ someVal.contents)
            ->Js.String2.replace(match[0], match[1])

      fn(someVal.contents)

    | None => switch str->Js.String2.match_(%re("/\|\|(.*?)\*\*(.*?)\*\*\|\|/")) {
      | Some(match) => 
      
        let match = match->Belt.Array.keepMap(x => x)

        let defaultValue = mainStr->getDefaultValue(match[2], allfiles)
        let nestedObject = mainStr->getObjectFunction(match[1], match[2], allfiles)

        someVal :=
          (defaultValue ++ "\n" ++ nestedObject ++ "\n" ++ someVal.contents)
            ->Js.String2.replace(match[0], match[1])

        fn(someVal.contents)
      | None => someVal.contents
      }
    }
  }
  fn(str)
}

let generateDecode = (type_, mainStr, allfiles) => {
  let (kind, blockScopeTypes) = mainStr->getRecordType(type_, allfiles)

  let convertedBlock = switch kind {
  | Normal =>
    blockScopeTypes->Js.String2.unsafeReplaceBy3(
      %re("/(@as+[A-Za-z0-9_()^@\\\\\"]+)?\s+([A-Za-z0-9_\"\\^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g"),
      (_, first, second, third, _, _) => {
        let first =
          Some(first)
          ->Belt.Option.getWithDefault("")
          ->Js.String2.replace("\"", "")
          ->Js.String2.replace("\"", "")
          ->Js.String2.replace("(", "")
          ->Js.String2.replace(")", "")
          ->Js.String2.replace("@as", "")

        let second = second->Js.String2.trim
        let third = third->Js.String2.trim
        let valueMapper = third->typeFunctionMapper(first->Js.String2.length > 0 ? first : second)
        `\t${second} : ${valueMapper}\n`
      },
    )
  | Varient => {
      let caseBlock = blockScopeTypes->Js.String2.split("|")->Js.Array2.reduce((acc, item) => {
          let item = item->Js.String2.replace("\n", "") -> Js.String2.trim
          acc ++ (` -> Belt.Option.orElse(${enumFunctionMapper(item)})`)
        }, "")++` -> Belt.Option.getExn`
      `  None
      \n${caseBlock}`
    }
  }

  let mapper = switch kind {
  | Normal =>
    `let itemToObjectMapper = dict => {
${convertedBlock}
}
 `
  | Varient =>
    `let get${type_->replaceFirstLetterUpper} = str => {
${convertedBlock}
}
 `
  }

  mapper->generateNestedObject(mainStr, allfiles)
}
