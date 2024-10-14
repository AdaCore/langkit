"""
Test script to generate/build the "test.lkt" language specification in the
current directory and optionally build and run test programs
(Ada/C/Python/OCaml/Java) with the generated library.
"""

import argparse
import shlex
import subprocess
import sys

from langkit.compile_context import CacheCollectionConf, LibraryEntity

from utils import Main, build_and_run


parser = argparse.ArgumentParser()

parser.add_argument("--generate-unparser", action="store_true")
parser.add_argument("--default-unparsing-config")

parser.add_argument("--default-unit-provider", type=LibraryEntity.from_fqn)
parser.add_argument("--symbol-canonicalizer", type=LibraryEntity.from_fqn)
parser.add_argument("--show-property-logging", action="store_true")
parser.add_argument("--case-insensitive", action="store_true")
parser.add_argument("--version")
parser.add_argument("--build-date")
parser.add_argument("--property-exceptions", action="append", default=[])
parser.add_argument("--cache-collection", nargs=2)

parser.add_argument("--py-script")
parser.add_argument("--py-args", type=shlex.split)
parser.add_argument("--gpr-mains", action="append", type=Main.parse)
parser.add_argument("--ocaml-main", type=Main.parse)
parser.add_argument("--java-main", type=Main.parse)
parser.add_argument("--ni-main", type=Main.parse)

parser.add_argument("--post-scripts", action="append")

# TODO (eng/libadalang/langkit#836) Parse our command line arguments and then
# clear them, so that our hack to pass test.py's arguments to manage.py scripts
# do not pick them up.
args = parser.parse_args()
sys.argv[1:] = []

# Turn the argument namespace into a dict for the keyword arguments that
# build_and_run expects.
kwargs = vars(args)
kwargs["property_exceptions"] = set(kwargs["property_exceptions"])

cache_collection_args = kwargs.pop("cache_collection")
cache_collection = None
if cache_collection_args:
    threshold, heuristic = cache_collection_args
    cache_collection = CacheCollectionConf(
        int(threshold),
        None if heuristic == "null" else LibraryEntity.from_fqn(heuristic),
    )
kwargs["cache_collection_conf"] = cache_collection

post_scripts = kwargs.pop("post_scripts") or []

build_and_run(lkt_file="test.lkt", **kwargs)

# Run post scripts before exitting
for filename in post_scripts:
    subprocess.check_call([sys.executable, filename])

print("Done")
