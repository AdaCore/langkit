import sys

import libfoolang


print("main.py: Running...")

text = b"""\
a aa a\\"a a\\\\a
\\
\tfoo
"""

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer=text)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("Tree:")
u.root.dump()
print("")

print("Tokens:")
t = u.first_token
while t is not None:
    print("  ", t)
    t = t.next
print("")

print("main.py: Done.")
