import json
import os


def main(ctx):
    if ctx is not None:
        package_json_filename = os.path.join(
            ctx.config.emission.library_directory,
            "vscode_ext",
            "package.json",
        )
        textmate_grammar_filename = os.path.join(
            ctx.config.emission.library_directory,
            "vscode_ext",
            "syntaxes",
            "Foo.tmGrammar.json",
        )
        if os.path.isfile(textmate_grammar_filename):
            with open(textmate_grammar_filename, "r") as f:
                print("Produced TextMate grammar:")
                print("-----")
                print(f.read())
                print("-----")
                print("")
            with open(package_json_filename, "r") as f:
                package_json = json.load(f)
                print('Produced package.json "grammars" field')
                print("-----")
                print(package_json["contributes"]["grammars"])
                print("-----")
            return
    print("No TextMate grammar produced...")
