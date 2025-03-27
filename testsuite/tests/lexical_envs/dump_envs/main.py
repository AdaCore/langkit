import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    buffer=b"""
        foo {
            bar {
                bar0
            }
            baz {}
            qux
        }
    """,
)
if u.diagnostics:
    for d in u.diagnostics:
        print("{}:{}".format(u.filename, d))
    sys.exit(1)
print("== Before PLE ==")
sys.stdout.flush()
u._dump_lexical_env()
print("")

print("== After PLE ==")
sys.stdout.flush()
u.populate_lexical_env()
u._dump_lexical_env()
print("")

print("main.py: Done.")
