import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    b"""
def a
    ()
    {x = 20}
    (100 + x)

def b
    (a)
    {}
    (15 + x)
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def parent_def(self):
    parent = self.parent
    while not isinstance(parent, libfoolang.Def):
        parent = parent.parent
    return parent


def ref_repr(self):
    return "<{} {} (from {})>".format(
        type(self).__name__, self.f_name.text, parent_def(self).f_name.text
    )


def var_repr(self):
    return "<{} {} (from {})>".format(
        type(self).__name__, self.f_name.text, parent_def(self).f_name.text
    )


libfoolang.Ref.__repr__ = ref_repr
libfoolang.Var.__repr__ = var_repr


for r in u.root.findall(libfoolang.Ref):
    print("{} -> {}".format(r, r.p_resolve))

print("main.py: Done.")
