import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


for prop in ("p_int_concat", "p_big_int_concat"):
    for a, b in [
        ([], []),
        ([1], []),
        ([], [2]),
        ([1], [2]),
        ([1, 2, 3], [4, 5]),
    ]:
        result = getattr(u.root, prop)(a, b)
        print(f"{prop}({a}, {b}) = {result}")

print('main.py: Done.')
