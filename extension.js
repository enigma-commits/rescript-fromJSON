const vscode = require("vscode");
const fs = require('fs');
const path = require('path');
const ignore = require('ignore');
const { languages, commands, workspace } = vscode;

const decodeFunction = require("./src/decodeFunction.bs");

class CodelensProvider {
  constructor() {
    this.codeLenses = [];
    this.regex = /type\s+(\w+)\s+=\s+\{/g;

    this._onDidChangeCodeLenses = new vscode.EventEmitter();
    this.onDidChangeCodeLenses = this._onDidChangeCodeLenses.event;

    vscode.workspace.onDidChangeConfiguration((_) => {
      this._onDidChangeCodeLenses.fire();
    });
  }

  provideCodeLenses(document, token) {
    if (
      vscode.workspace
        .getConfiguration("rescript-decode")
        .get("enableDecodeButton", true)
    ) {
      this.codeLenses = [];
      const regex = new RegExp(this.regex);
      const text = document.getText();

      let matches;
      while ((matches = regex.exec(text)) !== null) {
        const line = document.lineAt(document.positionAt(matches.index).line);
        const indexOf = line.text.indexOf(matches[0]);
        const position = new vscode.Position(line.lineNumber, indexOf);
        const range = document.getWordRangeAtPosition(
          position,
          new RegExp(this.regex)
        );
        if (range) {
          this.codeLenses.push(new vscode.CodeLens(range));
        }
      }
      return this.codeLenses;
    }
    return [];
  }

  resolveCodeLens(codeLens, token) {
    if (
      vscode.workspace
        .getConfiguration("rescript-decode")
        .get("enableDecodeButton", true)
    ) {
      const document = vscode.window.activeTextEditor.document;
      const currentLine = document.lineAt(codeLens.range.start.line).text;
      codeLens.command = {
        title: "Generate Decode",
        tooltip: "Click to generate decode function",
        command: "rescript-decode.generateDecode",
        arguments: [currentLine, false],
      };
      return codeLens;
    }
    return null;
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

  commands.registerCommand("rescript-decode.generateDecode", (args) => {
    console.log("initial came here");
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
      console.log("before generation came here");
      let result = decodeFunction.generateDecode(typeName, fileContents, files);
      console.log("after generation came here")
      let res = `open LogicUtils // Import Utilities from your Utils file
        ${result}
        `;
      console.log("after generation came here");
      vscode.env.clipboard.writeText(res).then(() => {
        vscode.window.showInformationMessage(
          `${typeName} : Generated code copied to clipboard.`
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
