import sys

import libfoolang


print("main.py: Running...")

old_node_repr = libfoolang.FooNode.__repr__


def node_repr(n):
    if n is None:
        return "<None>"
    elif n.is_a(libfoolang.Scope, libfoolang.Var):
        return n.f_name.text
    else:
        return old_node_repr(n)


libfoolang.FooNode.__repr__ = node_repr


def process(n, indent=""):
    print("{}processing {}".format(indent, n))
    indent += "  "
    if n.is_a(libfoolang.DefNodeList):
        for d in n:
            process(d, indent)
    if n.is_a(libfoolang.Scope):
        for d in n.f_defs:
            process(d, indent)
    elif n.is_a(libfoolang.Var):
        print("{}{} resolves to {}".format(indent, n, n.f_value.p_resolve))


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "foo",
    b"""
    scope_1 {
        a = scope_1
        error b {
            c {
                unreachable = scope_1
            }
        }
        d = scope_1
    }

    scope_2 {
        e = scope_1.a
        f = scope_1.b
        g = scope_1.c
        h = scope_1.d
    }
""",
)

if u.diagnostics:
    for d in u.diagnostics:
        print("{}".format(d))
    sys.exit(1)

u.populate_lexical_env()
process(u.root)

unreachable = u.root.find(
    lambda n: n.is_a(libfoolang.Var) and n.f_name.text == "unreachable"
)
print("{} resolves to {}".format(unreachable, unreachable.f_value.p_resolve))

print("main.py: Done.")
