"""
Check that the standalone library mode works as expected: make sure that not
only we can build a standalone library, but that we can also link a standalone
library and a regular one in the same program.
"""

import os.path
import subprocess

import langkit
from langkit.compile_context import CompileCtx
from langkit.libmanage import ManageScript
from langkit.utils import Language, SourcePostProcessor, add_to_path

from utils import jobs


HEADER = "--  CUSTOM HEADER\n"


class AdaSourcePostProcessor(SourcePostProcessor):
    def process(self, source):
        return HEADER + source


def manage(name: str, standalone: bool) -> ManageScript:
    name_low = name.lower()

    class Manage(ManageScript):

        def __init__(self):
            self._lib_name = name_low
            super().__init__()

        def create_context(self, args):
            return CompileCtx(
                lang_name=name,
                lexer=None,
                grammar=None,
                lkt_file=f"{name_low}.lkt",
                types_from_lkt=True,
                standalone=standalone,
                source_post_processors={
                    Language.ada: AdaSourcePostProcessor()
                },
            )

    return Manage()


# Generate code for both libraries
manages = [manage("Foo", standalone=True), manage("Bar", standalone=False)]
for m in manages:
    lib_short_name = m._lib_name
    lib_name = f"lib{lib_short_name}lang"
    build_dir = f"build-{lib_short_name}"
    m.run([
        "generate",
        "-vnone",
        "-wundocumented-nodes",
        f"--build-dir={build_dir}"
    ])
    langkit.reset()

    print(f"Check the presence of our custom header for {lib_name}")
    for filename in [
        f"{lib_name}.ads",
        f"{lib_name}_support.ads",
        f"{lib_name}_adasat.ads",
    ]:
        fullname = os.path.join(build_dir, "src", filename)
        if os.path.exists(fullname):
            with open(fullname, encoding="utf-8") as f:
                content = f.read()
            msg = (
                "custom header found"
                if content.startswith(HEADER) else
                "no match"
            )
        else:
            msg = "no such file"
        print(f"{filename}: {msg}")
    print("")

# Write a "setenv" script to make investigation easier
with open("setenv.sh", "w") as f:
    for m in manages:
        m.write_setenv(f)

# Add both libraries to the environment and build our main program
env = dict(os.environ)
for m in manages:
    m.setup_environment(lambda name, value: add_to_path(env, name, value))
subprocess.check_call(
    ["gprbuild", "-Pmain.gpr", "-q", "-p", f"-j{jobs}"], env=env
)

# Finally, check that it runs correctly
subprocess.check_call([os.path.join("obj", "main")])

print("Done")
