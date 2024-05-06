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

def old_a
def new_a

def old_b
def new_b

def old_c
def new_c

def old_d {
    def inner
}
def new_d
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

foo, old_a, new_a, old_b, new_b, old_c, new_c, old_d, new_d = u.root
rebound_twice = foo.p_rebind(old_b, new_b).p_rebind(old_a, new_a)

print("After appending rebindings one by one:")
print("-" * 50)
print()
print_rebindings(rebound_twice)

other_rebound = foo.p_rebind(old_d, new_d).p_rebind(old_c, new_c)
concat_test = other_rebound.p_concat_rebindings(rebound_twice)

# Rebindings concatenation used to reverse to ordering of rebindings
# from the RHS. This tests that the order is well-preserved.
# So in the output, `old_a` should appear before `old_b` and `old_c`
# before `old_d`.
print()
print("After concatenating rebindings:")
print("-" * 50)
print()
print_rebindings(concat_test)

# Shed rebindings from old_d's env
print()
print("After shedding rebindings from old_d's env:")
print("-" * 50)
print()
shed_test = concat_test.p_shed_rebindings(old_d)
print_rebindings(shed_test)

# Shed rebindings from old_d's inner env. Since the inner env is rebindable
# but the given rebinding stack doesn't rebind it, this used to return no
# rebinding, which is incorrect as old_d is still rebound in that stack and
# is a rebindable parent of the old_d's inner env. Instead, the returned
# rebindings should mention old_d.
print()
print("After shedding rebindings from old_d's inner env:")
print("-" * 50)
print()
shed_test = concat_test.p_shed_rebindings(old_d.f_inner[0])
print_rebindings(shed_test)

# Shed rebindings from old_b's env. The shedding mechanism used to only remove
# entries from the top of the stack until finding a matching rebindable env,
# without touching anything behind it, meaning in this case the rebindings
# returned would still contain the entries for old_c and old_d. Now, all
# entries are considered for shedding, and since old_c and old_d are not
# deemed relevant from neither old_b nor new_b, they will be removed.
print()
print("After shedding rebindings from old_b's env:")
print("-" * 50)
print()
shed_test = concat_test.p_shed_rebindings(old_b)
print_rebindings(shed_test)


print('main.py: Done.')
