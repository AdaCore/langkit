import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer=b"""
    def example example
    def example +example
    def +example example
    def +example +example
""")
if u.diagnostics:
    for d in u.diagnostics:
        print("{}:{}".format(u.filename, d))
    sys.exit(1)

for n in u.root:
    print("== {} ==".format(n))
    try:
        print(n.p_rebind)
    except libfoolang.PropertyError as exc:
        print("Exception: {}".format(exc))
    print("")

print("main.py: Done.")
