"""
Test script to generate/build the "test.lkt" language specification in the
current directory and optionally build and run test programs
(Ada/C/Python/OCaml/Java) with the generated library.
"""

import shlex
import subprocess
import sys

import yaml

from utils import Main, build_and_run


def decode_main(key):
    try:
        main_name = test_env[key]
    except KeyError:
        return None
    else:
        return Main.parse(main_name)


# Extract test configuration from "test.yaml"
with open("test.yaml") as f:
    test_env = yaml.safe_load(f)
config = test_env.get("config")

py_script = test_env.get("py_script")
py_args = test_env.get("py_args")
if py_args is not None:
    py_args = shlex.split(py_args)

gpr_mains = test_env.get("gpr_mains")
if gpr_mains:
    gpr_mains = [Main.parse(m) for m in gpr_mains]

ocaml_main = decode_main("ocaml_main")
java_main = decode_main("java_main")
ni_main = decode_main("ni_main")

post_scripts = test_env.get("post_scripts", [])

build_and_run(
    config=test_env.get("config"),
    py_script=py_script,
    py_args=py_args,
    gpr_mains=gpr_mains,
    ocaml_main=ocaml_main,
    java_main=java_main,
    ni_main=ni_main,
)

# Run post scripts before exitting
for filename in post_scripts:
    subprocess.check_call([sys.executable, filename])

print("Done")
