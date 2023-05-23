"""
Test script to compile all Lkt sources in the current directory up to code
emission and to print error messages (if any). If there is a "test.py" script
in the test directory, also execute it at the end of the driver execution.
"""

import argparse
import glob
import os.path
import subprocess
import sys

import langkit

from utils import emit_and_print_errors


parser = argparse.ArgumentParser()
parser.add_argument(
    "--generate-unparser",
    action="store_true",
    help="Enable the generation of the unparser",
)


args = parser.parse_args()


# Compile all *.lkt" file except the ones starting with "common", as they
# contain just common code for the other sources, but are not compilable alone.
tests = [f for f in glob.glob("*.lkt") if not f.startswith("common")]


for lkt_file in sorted(tests):
    print(f"== {lkt_file} ==")
    emit_and_print_errors(
        lkt_file=lkt_file,
        types_from_lkt=True,
        generate_unparser=args.generate_unparser,
    )
    langkit.reset()
    print("")


# If there is a "test.py" script in the test directory, run it
if os.path.exists("test.py"):
    print("== test.py ==")
    sys.stderr.flush()
    sys.stdout.flush()
    subprocess.check_call([sys.executable, "test.py"])
    print("")

print("lkt_compile: Done")
