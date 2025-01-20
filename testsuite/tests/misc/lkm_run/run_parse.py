# Command wrapper that always exits with status code 0, hides output from the
# subprocess and just prints if the subprocess succeeded.

import subprocess
import sys


program = sys.argv[1]
try:
    p = subprocess.run(
        sys.argv[1:],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
except FileNotFoundError:
    print("File not found:", program)
else:
    print(program, "succeeded" if p.returncode == 0 else "failed")
