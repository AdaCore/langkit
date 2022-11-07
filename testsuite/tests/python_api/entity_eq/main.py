import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""
(T) foo { a b }

(T U) old_bar { c d }
(T U) new_bar { e f }

(V) old_baz { g }
(V) new_baz { h }
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

foo, old_bar, new_bar, old_baz, new_baz = u.root
rebound1 = foo.p_rebind(old_bar, new_bar)
rebound2 = foo.p_rebind(old_bar, new_bar)
rebound3 = foo.p_rebind(old_baz, new_baz)

for e1, e2 in [
    (rebound1, rebound1),
    (rebound1, rebound2),
    (rebound1, rebound3),
]:
    print('{} vs. {}'.format(e1, e2))
    print('  identity check:   {}'.format(e1 is e2))
    print('  equality check:   {}'.format(e1 == e2))
    print('  inequality check: {}'.format(e1 != e2))

print('main.py: Done.')
