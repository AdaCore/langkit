"""
Test script to compile all Lkt sources in the current directory up to code
emission and to print error messages (if any). If there is a "test.py" script
in the test directory, also execute it at the end of the driver execution.
"""

import argparse
import glob
import os.path
import sys

import langkit
from langkit.diagnostics import WarningSet
from langkit.passes import PassManager

from utils import default_warning_set, emit_and_print_errors


parser = argparse.ArgumentParser()
parser.add_argument(
    "--all-warnings",
    action="store_true",
    help="Enable all Lkt compilation warnings",
)
PassManager.add_args(parser)
parser.add_argument(
    "lkt_files",
    nargs="*",
    help=(
        "Lkt sources to compile. If not provided, run through all *.lkt"
        " sources in the current directory expect the ones starting with"
        " 'common_'."
    ),
)


args = parser.parse_args()

warning_set = WarningSet() if args.all_warnings else default_warning_set

# Compile all *.lkt" file except the ones starting with "common", as they
# contain just common code for the other sources, but are not compilable alone.
tests = args.lkt_files or [
    f for f in glob.glob("*.lkt") if not f.startswith("common")
]


for lkt_file in sorted(tests):
    print(f"== {lkt_file} ==")
    ctx = emit_and_print_errors(
        lkt_file=lkt_file,
        warning_set=warning_set,
        pass_activations=args.pass_activations,
        types_from_lkt=True,
    )
    print("")

    # If there is a "test.py" script in the test directory, run it
    if os.path.exists("test.py"):
        print("== test.py ==")
        sys.stderr.flush()
        sys.stdout.flush()

        with open("test.py", "rb") as f:
            code = f.read()
        globs = {
            "__file__": "test.py",
            "__name__": "__main__",
        }
        exec(code, globs)

        # If this script defines a "main" function, call it with the
        # compilation context (or None if the compilation failed).
        if "main" in globs:
            globs["main"](ctx)

        print("")

    langkit.reset()


print("lkt_compile: Done")
