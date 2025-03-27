#! /usr/bin/env python3

import argparse
import sys


parser = argparse.ArgumentParser(
    description="Helper script to check if the Liblktlang Python bindings can"
    " be used. Exits with return code 0 if it can, exit with an error return"
    " code otherwise."
)
parser.add_argument(
    "--quiet",
    "-q",
    action="store_true",
    help="Do not print Liblktlang import issues on standard outputs",
)

args = parser.parse_args()

try:
    import liblktlang

    del liblktlang
except Exception:
    # Let the exception propagate in regular mode, and just exit with return
    # code 1 if in quiet mode.
    if args.quiet:
        sys.exit(1)
    else:
        print("Error while importing liblktlang:")
        sys.stdout.flush()
        raise
