import sys

import libfoolang


print("main.py: Running...")

text = b"""\
a = 1
b := 24
"""

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer=text)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)
u.root.dump()

print("main.py: Done.")
