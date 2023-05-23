"""
Check that running the "manage.py" script with no argument yields a clear error
message.
"""

import os.path
import subprocess
import sys

from e3.os.fs import which

import langkit


scripts_dir = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(langkit.__file__))),
    'scripts'
)


def locate_script(name):
    return which(name, default=os.path.join(scripts_dir, name))


# Create a dummy project
print("Creating dummy project...")
subprocess.check_call(
    [sys.executable, locate_script("create-project.py"), "Foo"]
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
