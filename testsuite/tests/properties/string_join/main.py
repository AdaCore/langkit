import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", "example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for strings, separator in [
    ([], ""),
    ([], "|"),
    ([""], ""),
    ([""], "|"),
    (["a"], ""),
    (["a"], "|"),
    (["ab", "cdef", "", "g"], ""),
    (["ab", "cdef", "", "g"], "|"),
    (["ab", "cdef", "", "g"], "<|>"),
]:
    strings_arg = [libfoolang.Str(value=s) for s in strings]
    result = u.root.p_join(strings_arg, separator)
    print(f"join({strings}, {repr(separator)}) = {repr(result)}")

print("main.py: Done.")
