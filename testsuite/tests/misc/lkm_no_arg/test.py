"""
Check that running "lkm" with no argument yields a clear error message.
"""

import subprocess
import sys


p = subprocess.run(
    [sys.executable, "-m", "langkit.scripts.lkm"],
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
