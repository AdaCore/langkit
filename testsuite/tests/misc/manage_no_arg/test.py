"""
Check that running the "manage.py" script with no argument yields a clear error
message.
"""

import os.path
import subprocess
import sys


# Create a dummy project
print("Creating dummy project...")
subprocess.check_call(
    [sys.executable, "-m", "langkit.scripts.create_project", "Foo"]
)

# Try to run its "manage.py" script
print("Running manage.py script with no argument:")
p = subprocess.run(
    [sys.executable, os.path.join("foo", "manage.py")],
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    encoding="utf-8",
)
print("Status code:", p.returncode)

# In order to preserve a stable test baseline, strip the actual help message
output = p.stdout.strip().splitlines()
for i, line in enumerate(output):
    if line.startswith("usage: "):
        output = output[:i]
        output.append("usage: ...")
print("Output:", "\n".join(output))

print('Done')
