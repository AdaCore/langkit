"""
Check that the "lkm generate" command does nothing unless one of its
inputs/outputs changed (or is missing).

Note that because of testing infrastructure constraints, this omits checks on
Langkit/Langkit_Support/AdaSAT changes.
"""

from contextlib import contextmanager
import os.path

from e3.fs import mkdir, rm

import langkit.scripts.create_project as create_project
import langkit.scripts.lkm as lkm


@contextmanager
def run(label, *argv):
    print(f"== {label} ==\n")

    # Clean up files from previous tests
    for f in ["build", "extensions", "langkit.yaml", "mylang"]:
        if os.path.exists(f):
            rm(f, recursive=True)

    # Create the test project and build it a first time (to create the cache).
    # Disable all outputs to avoid cluttering the test output.
    create_project.main(["Mylang"])
    with open("langkit.yaml", "a") as f:
        print("plugin_passes: [my_plugin.create_pass]", file=f)

    lkm.main(["generate", "-vnone"])

    # Let the caller change something
    yield

    # Run the incremental generation
    lkm.main(["generate", *argv])

    print()


with run("Build after no change"):
    pass

impl_body = "build/src/libmylanglang-implementation.adb"


def check_impl_body():
    with open(impl_body) as f:
        if f.readline():
            print(f"{impl_body} is not empty")
        else:
            print(f"{impl_body} is empty")
    print("")


with run("Build after an output file has changed"):
    print(f"Making {impl_body} empty...")
    with open(impl_body, "w"):
        pass
    check_impl_body()
check_impl_body()

# Make sure that, with --force/-f, even the emitter cache (used to determine
# whether it is worth re-writing a file or not) is disregarded. Since it relies
# on side-stored contents hashes, tampering generated files used not to trigger
# their re-generation even with --force/-f.
filename = "build/gdbinit.py"
dummy_contents = b"dummy contents\n"
with run("Build with -f", "-f"):
    assert os.path.isfile(filename)
    with open(filename, "wb") as f:
        f.write(dummy_contents)
with open(filename, "rb") as f:
    assert f.read() != dummy_contents

with run("Build with --portable-project", "--portable-project"):
    pass

with run("Build with Lkt source change"):
    with open("mylang/tokens.lkt", "a") as f:
        f.write("\n\n# Some additional comment\n")

with run("Build with new extension sources"):
    mkdir("extensions/src")
    with open("extensions/src/pkg.ads", "w") as f:
        f.write("package Pkg is\nend Pkg;\n")

with run("Build with new extension templates"):
    mkdir("extensions")
    with open("extensions/withed_projects", "w") as f:
        f.write('with "gnatcoll";\n')

with run("Build with innocuous langkit.yaml change"):
    with open("langkit.yaml", "a") as f:
        f.write("\nmanage_defaults:\n  enable_java: true\n")

with run("Build with significant langkit.yaml change"):
    with open("langkit.yaml", "a") as f:
        f.write("\nemission:\n  portable_project: true\n")

with run("Build with plugin data change"):
    with open("my_plugin_data.txt", "w") as f:
        f.write("Modified data\n")

with run("Build with plugin script change"):
    with open("my_plugin.py", "a") as f:
        f.write("\n# Additional comment\n")


print("Done")
