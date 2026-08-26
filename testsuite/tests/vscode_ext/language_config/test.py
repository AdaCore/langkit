import json
import os


def main(ctx):
    if ctx is not None:
        language_config_filename = os.path.join(
            ctx.config.emission.library_directory,
            "vscode_ext",
            "Foo-language-configuration.json",
        )
        package_json_filename = os.path.join(
            ctx.config.emission.library_directory,
            "vscode_ext",
            "package.json",
        )
        if os.path.isfile(language_config_filename):
            with open(language_config_filename, "r") as f:
                print("Copied language configuration:")
                print("-----")
                print(f.read())
                print("-----")
                print("")
            with open(package_json_filename, "r") as f:
                package_json = json.load(f)
                print('Produced package.json "languages" field')
                print("-----")
                print(package_json["contributes"]["languages"])
                print("-----")
        return
    print("No VSCode extension generated...")
