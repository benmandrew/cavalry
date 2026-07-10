# Cavalry syntax highlighting

A minimal VS Code extension providing a TextMate grammar for `.cav` files.
The grammar (`syntaxes/cavalry.tmLanguage.json`, scope `source.cavalry`) is a
plain TextMate grammar, so it is also consumable by Sublime Text, the
`vscode-textmate` library, and — if the language is registered upstream —
GitHub Linguist.

## Try it locally

```bash
# From the repository root, symlink the extension into VS Code, then reload:
ln -s "$PWD/editors/vscode" ~/.vscode/extensions/cavalry
```

Open any `.cav` file (e.g. `example.cav`) and highlighting is applied by file
extension. Use the *Developer: Inspect Editor Tokens and Scopes* command to see
the scope assigned to the token under the cursor.

## Packaging

```bash
npm install -g @vscode/vsce
cd editors/vscode
vsce package        # produces cavalry-0.1.0.vsix
```
