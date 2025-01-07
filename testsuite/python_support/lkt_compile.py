"""
Test script to compile all Lkt sources in the current directory up to code
emission and to print error messages (if any). If there is a "test.py" script
in the test directory, also execute it at the end of the driver execution.
"""

import argparse
import glob
import os.path
import sys

import yaml

import langkit

from utils import emit_and_print_errors


# Extract test configuration from "test.yaml"
with open("test.yaml") as f:
    test_env = yaml.safe_load(f)
config = test_env.get("config")

parser = argparse.ArgumentParser()
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

# Compile all *.lkt" file except the ones starting with "common", as they
# contain just common code for the other sources, but are not compilable alone.
tests = args.lkt_files or [
    f for f in glob.glob("*.lkt") if not f.startswith("common")
]


for lkt_file in sorted(tests):
    print(f"== {lkt_file} ==")
    ctx = emit_and_print_errors(lkt_file=lkt_file, config=config)
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
