import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()


for src_buffer in [
    "()",
    "(a)",
    "(foo)",
    "(a foo b)",
    "(a b c)",
    "(foo foo)",
]:
    print("#", src_buffer)
    u = ctx.get_from_buffer("main.txt", src_buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    print("any_foo =", u.root.p_any_foo)
    print("all_foo =", u.root.p_all_foo)
    print("")

print('main.py: Done.')
