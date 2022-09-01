import sys

import libfoolang


print('main.py: Running...')


def load_unit(name, buffer):
    u = ctx.get_from_buffer(name, buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    return u


ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)

u1 = load_unit('main1.txt', b'a b c')
n = u1.root.find(libfoolang.Name)
e1 = n.p_with_md(True, False)
e2 = n.p_with_md(True, True)
e3 = n.p_with_md(False, True)
print(f"e1 == e2: {e1 == e2}")
print(f"e1 == e3: {e1 == e3}")
print(f"hash(e1) == hash(e2): {hash(e1) == hash(e2)}")
print(f"hash(e1) == hash(e3): {hash(e1) == hash(e3)}")
