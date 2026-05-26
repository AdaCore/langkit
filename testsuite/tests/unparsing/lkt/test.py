"""
Test that the Lkt-to-JSON and JSON-to-Lkt translations for the unparsing
configuration work as expected.
"""

import difflib
import glob
import json
import os.path
import shlex
import sys

from langkit.scripts.unparsing2lkt import main


for json_filename in sorted(glob.glob("*.json") + glob.glob("*/*.json")):
    # The JSON-to-Lkt translation assumes valid JSON inputs, so skip invalid
    # tests.
    if os.path.basename(json_filename).startswith(
        "invalid_"
    ) or json_filename in ("missing_node_configs.json", "no_such_file.json"):
        continue

    def error(msg):
        print(f"== {json_filename} ==")
        print()
        print(msg)
        print()

    def run(*args):
        sys.stdout.flush()
        status = main(args)
        if status != 0:
            error(
                f"unparsing2lkt {shlex.join(args)} failed with status code"
                f" {status}"
            )
            return True
        else:
            return False

    # Translate the JSON file to Lkt
    if run("--to-lkt", json_filename, "-o", "tmp.lkt"):
        continue

    # Translate the Lkt file back to JSON
    if run("--to-json", "tmp.lkt", "-o", "tmp.json"):
        continue

    # Reformat the original JSON file so that we compare the actual data, not
    # its formatting.
    with open(json_filename) as f:
        original_doc = json.load(f)
    original_json = json.dumps(original_doc, indent=2, sort_keys=True)
    original_lines = [line + "\n" for line in original_json.splitlines()]

    # Ensure the translation did not alter the configuration
    with open("tmp.json") as f:
        translated_lines = f.readlines()

    diff = list(
        difflib.unified_diff(
            original_lines,
            translated_lines,
            fromfile=json_filename,
            tofile="tmp.json",
        )
    )
    if diff:
        error("".join(diff))

print("Done")
