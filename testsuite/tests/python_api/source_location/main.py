import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo.txt", b"foobar")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

sloc = u.root.sloc_range.end
print(f"p_id_sloc({sloc}) = {u.root.p_id_sloc(sloc)}")

print("main.py: Done.")
