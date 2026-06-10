import os


def main(ctx):
    if ctx is not None:
        generated_filename = os.path.join(
            ctx.config.emission.library_directory,
            "foo.tmLanguage.json",
        )
        if os.path.isfile(generated_filename):
            with open(os.path.join("build", "foo.tmLanguage.json"), "r") as f:
                print("Produced TextMate grammar:")
                print("-----")
                print(f.read())
                print("-----")
                return
    print("No TextMate grammar produced...")
