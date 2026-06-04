<%
from json import dumps
lang_name = ctx.config.library.language_name.camel
ext_config = ctx.config.vscode_ext
%>
{
  "name": ${dumps(ext_config.name)},
  "displayName": ${dumps(ext_config.display_name)},
  "description": ${dumps(ext_config.description)},
  "publisher": ${dumps(ext_config.publisher)},
% if ext_config.repository is not None:
  "repository": {
    "type": "git",
    "url": ${dumps(ext_config.repository)}
  }
% endif
  "version": ${dumps(ext_config.version)},
  "engines": {
    "vscode": "^1.92.0"
  },
  "categories": [
    "Programming Languages"
  ],
  "activationEvents": [
    "onLanguage:${lang_name}"
  ],
  "main": "./out/extension.js",
  "contributes": {
    "languages": [
      {
        "id": "${lang_name}",
        "extensions": [
          ${", ".join(
            f'"{ext}"' for ext in ctx.config.language_server.file_extensions
          )}
        ]
      }
    ],
    "configuration": {
      "type": "object",
      "title": "${lang_name} Language Server configuration"
    },
    "commands": [
    ]
  },
  "scripts": {
    "vscode:prepublish": "npm run compile",
    "compile": "tsc -p ./",
    "lint": "eslint src --ext ts",
    "package": "npx @vscode/vsce package"
  },
  "dependencies": {
      "vscode-languageclient": "^9.0.1"
  },
  "devDependencies": {
    "@types/vscode": "^1.92.0",
    "@types/mocha": "^10.0.7",
    "@types/node": "20.x",
    "@typescript-eslint/eslint-plugin": "^7.14.1",
    "@typescript-eslint/parser": "^7.11.0",
    "eslint": "^8.57.0",
    "typescript": "^5.4.5",
    "@vscode/vsce": "^2.15.0"
  }
}
