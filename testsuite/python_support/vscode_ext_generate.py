"""
Test script to generate a VSCode extension in the current directory and to
print error messages (if any). If there is a "test.py" script in the test
directory, also execute it at the end of the driver execution.
"""

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

# Add the current working directory as a source dir
test_config = derive_config(
    config,
    {
        "language_server": {
            "file_extensions": [".foo"],
        },
        "lkt_spec": {"source_dirs": [".", python_support_dir]},
    },
)

# Run the VSCode extension code generation
ctx = emit_and_print_errors(
    config=test_config,
    compilation_mode=langkit.compile_context.CompilationMode.generate_ext,
)
print("")
run_test_py(ctx)
langkit.reset()

print("vscode_ext_generate: Done")
