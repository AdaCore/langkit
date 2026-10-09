import os


def main(ctx):
    if ctx is not None:
        readme_filename = os.path.join(
            ctx.config.emission.library_directory,
            "vscode_ext",
            "README.md",
        )
        if os.path.isfile(readme_filename):
            with open(readme_filename, "r") as f:
                print("Copied README content:")
                print("-----")
                print(f.read())
                print("-----")
                print("")
        return
    print("No VSCode extension generated...")
