import sys

import libfoolang


print("main.py: Running...")

text = b"""\
def a = 0x1
DeF B = 0X2F
"""

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer=text)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for decl in u.root:
    print("== {} ==".format(decl))
    for s in ("a", "A", "b", "B"):
        print("p_matches({}) = {}".format(s, decl.f_name.p_matches(s)))
    print("")

print("main.py: Done.")
