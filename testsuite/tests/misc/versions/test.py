"""
Check that version numbers are available from all APIs.
"""

import langkit
from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode

from utils import build_and_run


def run(label, **kwargs):
    print("== {} ==".format(label))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

    try:
        build_and_run(
            lkt_file="expected_concrete_syntax.lkt",
            version="1.version.number",
            build_date="build.date.number",
            types_from_lkt=True,
            **kwargs
        )
    except DiagnosticError:
        print("DiagnosticError: skipping...")
    langkit.reset()
    print("")


run("Conflict on version",
    additional_make_args=["--version=1.something"],
    full_error_traces=False)
run("Conflict on build date",
    additional_make_args=["--build-date=something"],
    full_error_traces=False)
run("Build and run test programs", py_script="main.py", gpr_mains=["main.adb"])

print("Done")
