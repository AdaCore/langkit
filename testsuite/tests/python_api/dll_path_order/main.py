"""
Check that the Windows-specific `os.add_dll_directory` Python API is correctly
used in Python bindings to import the implementation DLL. It used to process
PATH in the wrong order, making last directorise have precedence over previous
directories.
"""

import os.path
import subprocess


print("main.py: Running...")

# Build a dummy liblktlang shared object and put it last in the shared object
# search order, then make it available *last* (it should not hide the
# libfoolang build for this testcase, already in the PATH).
subprocess.check_call(["gprbuild", "-P", "dummy_lib.gpr", "-q"])
os.environ["PATH"] += ";" + os.path.abspath("dummy_lib\\lib")

# Now thy to import libfoolang: it used to fail
import libfoolang


# The following is just a sanity check that the imported library is working
# correctly.
ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo.txt", b"example")
u.root.dump()

print("main.py: Done.")
