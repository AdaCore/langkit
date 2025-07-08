"""
Check that block expressions are parsed as expected.
"""

import glob

import liblktlang as lkt


ctx = lkt.AnalysisContext()

for filename in sorted(glob.glob("*.lkt")):
    print(f"== {filename} ==")
    print("")
    u = ctx.get_from_file(filename)
    if u.diagnostics:
        for d in u.diagnostics:
            print(u.format_gnu_diagnostic(d))
        print("")
    if u.root is None:
        print("No root node")
    else:
        u.root.dump()
    print("")

print("Done")
