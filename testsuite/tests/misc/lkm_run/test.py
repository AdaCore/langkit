"""
Check that the "lkm run" command works as expected.
"""

import os.path
import sys

import langkit.scripts.create_project as create_project
import langkit.scripts.lkm as lkm


# Create a fresh project and build it with a non-default build directory
build_dir_arg = f"--build-dir={os.path.abspath('foo-build')}"
create_project.main(["Mylang"])
lkm.main(["make", "-vnone", build_dir_arg])

for label, opts in [
    # Try to run the parse executable without the right build dir: tt
    # should fail, as the executable is not in the PATH.
    ("Sanity check", []),

    # With the right build dir however, the executable will be in the PATH.
    # This proves that 1) "lkm run" do sets the environment and 2) does it
    # according to its own arguments (here: --build-dir).
    ("Correct build dir", [build_dir_arg]),
]:
    print("#", label)
    sys.stdout.flush()
    lkm.main(
        ["run", *opts, sys.executable, "run_parse.py", "libmylanglang_parse"]
    )
    print()

print('Done')
