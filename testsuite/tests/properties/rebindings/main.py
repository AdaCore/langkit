import sys

import libfoolang


def def_repr(n):
    return '<{} {} {}>'.format(
        type(n).__name__,
        n.p_name,
        n.sloc_range
    )


libfoolang.DefNode.__repr__ = def_repr


def process(index, n):
    def wrapped_eval(lambda_fn):
        try:
            return lambda_fn()
        except libfoolang.PropertyError as exc:
            return '<{}: {}>'.format(type(exc).__name__, exc)

    print('Processing {} ({})'.format(n, index))
    parent = wrapped_eval(lambda: n.p_parent_rebindings)
    new = wrapped_eval(lambda: n.p_new)
    old = wrapped_eval(lambda: n.p_old)
    print('  parent = {}'.format(parent))
    print('  new = {}'.format(new))
    print('  old = {}'.format(old))

    # Computing "parent" raised a PropertyError => "parent" is a string
    if not isinstance(parent, str):
        process(index + 1, parent)


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
rebound_once = foo.p_rebind(old_bar, new_bar)
rebound_twice = rebound_once.p_rebind(old_baz, new_baz)

process(0, rebound_twice)

print('main.py: Done.')
