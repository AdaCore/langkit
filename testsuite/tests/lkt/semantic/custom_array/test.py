"""
Test defining custom array types.
"""

import subprocess


print("main.py: defining a second @array type.", flush=True)

subprocess.check_call(
    ["lkt_toolbox", "-C", "test.lkt"], cwd="invalid_second_array_type"
)

print("main.py: replacing prelude's @array type.", flush=True)

subprocess.check_call(
    ["lkt_toolbox", "test.lkt"], cwd="valid_custom_array_type"
)

print("main.py: missing definition of @array type.", flush=True)

subprocess.check_call(
    ["lkt_toolbox", "-C", "test.lkt"], cwd="invalid_missing_array_type"
)

print("main.py: wrong definition of @array type.", flush=True)

subprocess.check_call(
    ["lkt_toolbox", "-C", "test.lkt"], cwd="invalid_wrong_array_type"
)

print("main.py: Done")
