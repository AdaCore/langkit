"""
Check that "lkm build" and "lkm run" do not try to import the liblktlang Python
module, as it would break the bootstrap procedure for Liblktlang.
"""

import subprocess
import sys

import langkit.scripts.create_project as create_project
import langkit.scripts.lkm as lkm


# Create a dummy project on which we can run lkm, then generate its sources in
# a subprocess so that this one does not try to import liblktlang yet.
create_project.main(["Foo"])
subprocess.check_call(
    [sys.executable, "-m", "langkit.scripts.lkm", "generate", "-vnone"]
)


def run(label, argv, uses_liblktlang=False):
    # Run the requested lkm command. Flush stdout so that the output of
    # subprocesses show up after the label.
    print(label)
    sys.stdout.flush()
    lkm.main(argv)

    # Check the import status of liblktlang
    if uses_liblktlang:
        assert "liblktlang" in sys.modules
    else:
        assert "liblktlang" not in sys.modules

    print()


# Since they do not need to analyze lkm code, "lkm build" and "lkm run" are not
# supposed to import liblktlang.
run("lkm build", ["build", "-vnone"])
run("lkm run", ["run", sys.executable, "hello_world.py"])

# As a sanity check, run "lkm generate", which is supposed to import liblktlang
run("lkm generate", ["generate", "-vnone"], uses_liblktlang=True)

print('Done')
