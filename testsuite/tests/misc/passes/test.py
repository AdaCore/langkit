"""
Check the handling of optional and plugin passes.
"""

import sys

import langkit
import langkit.scripts.lkm as lkm


def run(label, args, passes=None, expect_error=False):
    print(f"== {label} ==")
    print()
    sys.stdout.flush()

    # If we do not expect an error display the stack trace to ease debugging
    if not expect_error:
        args.append("--full-error-traces")

    if not passes:
        passes = ["my_py_lib.pass_a", "my_py_lib.pass_b", "my_py_lib.pass_c"]

    result = lkm.main_no_exit(
        [*args, "-vnone", *[f"--plugin-pass={p}" for p in passes]]
    )
    if result:
        print()
        print("! Error exit code")
    print()
    langkit.reset()


# List command
run("List optional passes", ["list-optional-passes"])

# Pipeline control
run("Run the default pipeline", ["generate"])
run("Run the check-only pipeline", ["generate", "--check-only"])
run(
    "Disable non-optional pass_a",
    ["generate", "--pass-off=pass_a"],
    expect_error=True,
)
run(
    "Disable non-existing pass_z",
    ["generate", "--pass-off=pass_z"],
    expect_error=True,
)
run("Enable optional pass_b", ["generate", "--pass-on=pass_b"])
run("Disable optional pass_c", ["generate", "--pass-off=pass_c"])
run(
    "Disable+enable pass_c",
    ["generate", "--pass-off=pass_c", "--pass-on=pass_c"],
)
run(
    "Enable+disable pass_c",
    ["generate", "--pass-on=pass_c", "--pass-off=pass_c"],
)

# Plugin passes error cases
run(
    "Invalid --plugin-pass format",
    ["generate"],
    passes=["foo"],
    expect_error=True,
)
run(
    "Non-existing module for --plugin-pass",
    ["generate"],
    passes=["no_such_module.foo"],
    expect_error=True,
)
run(
    "Non-existing callable for --plugin-pass",
    ["generate"],
    passes=["my_py_lib.no_such_callable"],
    expect_error=True,
)
run(
    "Invalid callable for --plugin-pass (wrong signature)",
    ["generate"],
    passes=["my_py_lib.invalid_cb1"],
    expect_error=True,
)
run(
    "Invalid callable for --plugin-pass (wrong return type)",
    ["generate"],
    passes=["my_py_lib.invalid_cb2"],
    expect_error=True,
)

print("Done")
