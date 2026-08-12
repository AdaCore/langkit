"""
Test script to compile all Lkt sources in the current directory up to code
emission and to print error messages (if any). If there is a "test.py" script
in the test directory, also execute it at the end of the driver execution.
"""

import argparse
import glob
import os.path
import yaml

import langkit

from utils import (
    derive_config,
    emit_and_print_errors,
    python_support_dir,
    run_test_py,
)


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
if args.lkt_files:
    tests = list(args.lkt_files)
else:
    tests = [
        f for f in glob.glob("*.lkt") if not f.startswith("common")
    ] + glob.glob("*/test.lkt")


for lkt_file in sorted(tests):
    # Harmonize pathnames so that output on Windows is identical to output on
    # Unix systems.
    lkt_file = lkt_file.replace("\\", "/")
    print(f"== {lkt_file} ==")

    test_config = derive_config(
        config,
        {
            "lkt_spec": {
                "source_dirs": [
                    ".",
                    os.path.dirname(lkt_file),
                    python_support_dir,
                ]
            }
        },
    )

    ctx = emit_and_print_errors(lkt_file=lkt_file, config=test_config)
    print("")
    run_test_py(ctx)
    langkit.reset()


print("lkt_compile: Done")
