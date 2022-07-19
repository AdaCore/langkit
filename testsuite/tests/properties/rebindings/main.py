import sys

import libfoolang


def block_repr(n):
    return f"<{type(n).__name__} {n.f_name.text}>"


libfoolang.Block.__repr__ = block_repr


def exc_repr(exc):
    return f"<{type(exc).__name__}: {exc}>"


def print_rebindings(n):

    def wrapped_eval(lambda_fn):
        try:
            return lambda_fn()
        except libfoolang.PropertyError as exc:
            return exc_repr(exc)

    # Print each element of the rebindings chain in "n"
    while True:
        print(f"{n}:")
        new = wrapped_eval(lambda: n.p_new)
        old = wrapped_eval(lambda: n.p_old)
        print(f"  new = {new}")
        print(f"  old = {old}")

        try:
            n = n.p_parent_rebindings
        except libfoolang.PropertyError as exc:
            print(f"  no parent: {exc_repr(exc)}")
            break


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"""
def foo

def old_bar
def new_bar

def old_baz
def new_baz
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

foo, old_bar, new_bar, old_baz, new_baz = u.root
rebound_once = foo.p_rebind(old_bar, new_bar)
rebound_twice = rebound_once.p_rebind(old_baz, new_baz)

print_rebindings(rebound_twice)

print('main.py: Done.')
