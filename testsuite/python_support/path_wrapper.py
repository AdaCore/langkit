#! /usr/bin/env python3

"""
Command-line wrapper to run a Python script with directories from the PATH
environment variable included in the shared libraries lookup system.

We use this wrapper systems to avoid repeating the same logic in each testcase:
since Python 3.8 on Windows, the environment variable that is supposed to give
access to DLLs (PATH) does not actually affect ctypes' DLL lookup system.

Users of Langkit-generated libraries are supposed to package necessary DLLs in
their Python wheel, so they do not need this. However, creating wheels is too
heavy for tests, so we need to use "os.add_dll_directory" to affect ctypes's
lookup system.
"""

import os
import os.path
import sys


if hasattr(os, "add_dll_directory"):
    # Make sure first directories in PATH have precedence over last directories
    # during DLL lookup.
    for d in reversed(os.environ["PATH"].split(os.path.pathsep)):
        if os.path.exists(d):
            os.add_dll_directory(d)

# Now run the script passed as argument. Remove "path_wrapper.py" from the list
# of arguments so that the wrapped script sees "sys.argv" as if it was run
# directly.
os.environ["PATH_WRAPPER"] = sys.argv.pop(0)
script = sys.argv[0]
sys.path.insert(0, os.path.dirname(script))
with open(script, "rb") as f:
    code = f.read()
globs = {
    "__file__": script,
    "__name__": "__main__",
}
exec(code, globs)
