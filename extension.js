const vscode = require("vscode");
const fs = require('fs');
const path = require('path');
const ignore = require('ignore');
const { languages, commands, workspace } = vscode;

const decodeFunction = require("./src/decodeFunction.bs");
const encodeFunction = require("./src/encodeFunction");

class CodelensProvider {
  constructor() {
    this.codeLenses = [];
    this.regexDecode = /type\s+(\w+)\s+=\s+\{/g;
    this.regexEncode = /type\s+(\w+)\s+/g;

    this._onDidChangeCodeLenses = new vscode.EventEmitter();
    this.onDidChangeCodeLenses = this._onDidChangeCodeLenses.event;

    vscode.workspace.onDidChangeConfiguration((_) => {
      this._onDidChangeCodeLenses.fire();
    });
  }

  provideCodeLenses(document, token) {
    const enableDecodeButton = vscode.workspace
      .getConfiguration("rescript-decode")
      .get("enableDecodeButton", true);
    const enableEncodeButton = vscode.workspace
      .getConfiguration("rescript-decode")
      .get("enableEncodeButton", true);
    const enableEncodeDecodeButton = vscode.workspace
      .getConfiguration("rescript-decode")
      .get("enableEncodeDecodeButton", true);

    if (enableDecodeButton || enableEncodeButton || enableEncodeDecodeButton) {
      this.codeLenses = [];
      const text = document.getText();

      if (enableDecodeButton) {
        let matchesDecode;
        const regexDecode = new RegExp(this.regexDecode);
        while ((matchesDecode = regexDecode.exec(text)) !== null) {
          const line = document.lineAt(document.positionAt(matchesDecode.index).line);
          const indexOf = line.text.indexOf(matchesDecode[0]);
          const position = new vscode.Position(line.lineNumber, indexOf);
          const range = document.getWordRangeAtPosition(
            position,
            new RegExp(this.regexDecode)
          );
          if (range) {
            this.codeLenses.push(
              new vscode.CodeLens(range, {
                title: "Generate Decode",
                tooltip: "Click to generate decode function",
                command: "rescript-decode.generateDecode",
                arguments: [line.text],
              })
            );
          }
        }
      }

      if (enableEncodeButton) {
        let matchesEncode;
        const regexEncode = new RegExp(this.regexEncode);
        while ((matchesEncode = regexEncode.exec(text)) !== null) {
          const line = document.lineAt(document.positionAt(matchesEncode.index).line);
          const indexOf = line.text.indexOf(matchesEncode[0]);
          const position = new vscode.Position(line.lineNumber, indexOf);
          const range = document.getWordRangeAtPosition(
            position,
            new RegExp(this.regexEncode)
          );
          if (range) {
            this.codeLenses.push(
              new vscode.CodeLens(range, {
                title: "Generate Encode",
                tooltip: "Click to generate encode function",
                command: "rescript-decode.generateEncode",
                arguments: [line.text],
              })
            );
          }
        }
      }

      if (enableEncodeDecodeButton) {
        let matchesEncodeDecode;
        const regexEncodeDecode = new RegExp(this.regexEncode);
        while ((matchesEncodeDecode = regexEncodeDecode.exec(text)) !== null) {
          const line = document.lineAt(document.positionAt(matchesEncodeDecode.index).line);
          const indexOf = line.text.indexOf(matchesEncodeDecode[0]);
          const position = new vscode.Position(line.lineNumber, indexOf);
          const range = document.getWordRangeAtPosition(
            position,
            new RegExp(this.regexEncode)
          );
          if (range) {
            this.codeLenses.push(
              new vscode.CodeLens(range, {
                title: "Generate Encode & Decode",
                tooltip: "Click to generate encode and decode functions",
                command: "rescript-decode.generateEncodeDecode",
                arguments: [line.text],
              })
            );
          }
        }
      }

      return this.codeLenses;
    }
    return [];
  }

  resolveCodeLens(codeLens, token) {
    const document = vscode.window.activeTextEditor.document;
    const currentLine = document.lineAt(codeLens.range.start.line).text;

    if (
      vscode.workspace
        .getConfiguration("rescript-decode")
        .get("enableDecodeButton", true) &&
      codeLens.command.command === "rescript-decode.generateDecode"
    ) {
      codeLens.command = {
        title: "Generate Decode",
        tooltip: "Click to generate decode function",
        command: "rescript-decode.generateDecode",
        arguments: [currentLine, false],
      };
    } else if (
      vscode.workspace
        .getConfiguration("rescript-decode")
        .get("enableEncodeButton", true) &&
      codeLens.command.command === "rescript-decode.generateEncode"
    ) {
      codeLens.command = {
        title: "Generate Encode",
        tooltip: "Click to generate encode function",
        command: "rescript-decode.generateEncode",
        arguments: [currentLine, false],
      };
    } else if (
      vscode.workspace
        .getConfiguration("rescript-decode")
        .get("enableEncodeDecodeButton", true) &&
      codeLens.command.command === "rescript-decode.generateEncodeDecode"
    ) {
      codeLens.command = {
        title: "Generate Encode & Decode",
        tooltip: "Click to generate encode and decode functions",
        command: "rescript-decode.generateEncodeDecode",
        arguments: [currentLine, false],
      };
    }

    return codeLens;
  }
}

function readDecodeIgnoreFile(rootDir) {
  const ignoreFilePath = path.join(rootDir, '.decodeIgnore');
  if (fs.existsSync(ignoreFilePath)) {
    const ignoreContent = fs.readFileSync(ignoreFilePath, 'utf8');
    return ignore().add(ignoreContent);
  } else {
    return null;
  }
}

function readFilesRecursively(dirPath) {
  let files = {}
  const ig = readDecodeIgnoreFile(dirPath);

  function readDirRecursively(currentPath) {
      const items = fs.readdirSync(currentPath);

      items.forEach(item => {
          const itemPath = path.join(currentPath, item);
          const relativePath = path.relative(dirPath, itemPath);
          const stats = fs.statSync(itemPath);

          if (ig && ig.ignores(relativePath)) {
            console.log(`Ignoring: ${relativePath}`);
            return;
          }
    

          if (stats.isDirectory()) {
              readDirRecursively(itemPath);
          } else if (stats.isFile()) {
              const content = fs.readFileSync(itemPath, 'utf8');
              files[path.parse(itemPath).name] = content;
          }
      });
  }

  readDirRecursively(dirPath);
  return files;
}

function activate(context) {
  const codelensProvider = new CodelensProvider();

  languages.registerCodeLensProvider("*", codelensProvider);
  const workspaceFolders = vscode.workspace.workspaceFolders;

  if (!workspaceFolders) {
      vscode.window.showErrorMessage('No workspace folder is open');
      return;
  }
  const workspaceFolder = workspaceFolders[0].uri.fsPath;
  const files = readFilesRecursively(workspaceFolder);

  commands.registerCommand("rescript-decode.enableDecodeButton", () => {
    workspace
      .getConfiguration("rescript-decode")
      .update("enableDecodeButton", true, true);
  });

  commands.registerCommand("rescript-decode.enableEncodeButton", () => {
    workspace
      .getConfiguration("rescript-decode")
      .update("enableEncodeButton", true, true);
  });

  commands.registerCommand("rescript-decode.enableEncodeDecodeButton", () => {
    workspace
      .getConfiguration("rescript-decode")
      .update("enableEncodeDecodeButton", true, true);
  });

  commands.registerCommand("rescript-decode.generateEncode", (args) => {
    const regex = /type\s+(\w+)\s+=/; // Regular expression to match the type name
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage("No active editor found.");
      return;
    }
    const match = args.match(regex);
    if (match) {
      const typeName = match[1];
      const fileContents = editor.document.getText();
      let result = encodeFunction.generateEncode(typeName, fileContents, files);
      let res = `${result}`;
      vscode.env.clipboard.writeText(res).then(() => {
        vscode.window.showInformationMessage(
          `${typeName} : Generated code copied to clipboard.`
        );
      });
    } else {
      vscode.window.showErrorMessage("No Type Found");
    }
  });

  commands.registerCommand("rescript-decode.generateDecode", (args) => {
    const regex = /type\s+(\w+)\s+=/; // Regular expression to match the type name
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage("No active editor found.");
      return;
    }
    const match = args.match(regex);
    if (match) {
      const typeName = match[1];
      const fileContents = editor.document.getText();
      let result = decodeFunction.generateDecode(typeName, fileContents, files);
      let res = `${result}`;
      vscode.env.clipboard.writeText(res).then(() => {
        vscode.window.showInformationMessage(
          `${typeName} : Generated code copied to clipboard.`
        );
      });
    } else {
      vscode.window.showErrorMessage("No Type Found");
    }
  });

  commands.registerCommand("rescript-decode.generateEncodeDecode", (args) => {
    const regex = /type\s+(\w+)\s+=/; // Regular expression to match the type name
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage("No active editor found.");
      return;
    }
    const match = args.match(regex);
    if (match) {
      const typeName = match[1];
      const fileContents = editor.document.getText();
      let decodeResult = decodeFunction.generateDecode(typeName, fileContents, files);
      let encodeResult = encodeFunction.generateEncode(typeName, fileContents, files);
      let res = `${decodeResult}\n${encodeResult}`;
      vscode.env.clipboard.writeText(res).then(() => {
        vscode.window.showInformationMessage(
          `${typeName} : Generated encode and decode code copied to clipboard.`
        );
      });
    } else {
      vscode.window.showErrorMessage("No Type Found");
    }
  });
}

function deactivate() {}

module.exports = {
  activate,
  deactivate,
};
