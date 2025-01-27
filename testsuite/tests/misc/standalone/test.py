"""
Check that the standalone library mode works as expected: make sure that not
only we can build a standalone library, but that we can also link a standalone
library and a regular one in the same program.
"""

import os.path
import subprocess
import sys

import langkit
import langkit.scripts.lkm as lkm
from langkit.utils import add_to_path

from my_pp import HEADER
from utils import jobs


# Write a "setenv" script to make investigation easier, and compute the actual
# derived env for both libraries.
setenv_file = open("setenv.sh", "w")
env = dict(os.environ)

# Build and install both libraries
for lib_short_name in ["foo", "bar"]:
    config_args = ["-c", os.path.join(lib_short_name, "langkit.yaml")]
    m = lkm.Manage()

    lib_name = f"lib{lib_short_name}lang"
    status = m.run_no_exit(
        ["make", "-vnone", "-wundocumented-nodes", *config_args]
    )
    assert status == 0, str(status)

    print(f"Check the presence of our custom header for {lib_name}")
    for filename in [
        f"{lib_name}.ads",
        f"{lib_name}_support.ads",
        f"{lib_name}_adasat.ads",
    ]:
        fullname = os.path.join(lib_short_name, "build", "src", filename)
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

    m.write_setenv(setenv_file)
    m.setup_environment(lambda name, value: add_to_path(env, name, value))

    langkit.reset()

# Build our main program, which uses both libraries
subprocess.check_call(
    ["gprbuild", "-Pmain.gpr", "-q", "-p", f"-j{jobs}"], env=env
)

# Finally, check that it runs correctly
sys.stdout.flush()
subprocess.check_call([os.path.join("obj", "main")])

print("Done")
