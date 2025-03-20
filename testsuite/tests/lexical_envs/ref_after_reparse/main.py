import os.path
import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()


def load_unit(name):
    u = ctx.get_from_file(name)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    return u


def name_img(node):
    return node.f_name.text


def Name_repr(self):
    return "<{} {} {}:{}>".format(
        type(self).__name__,
        name_img(self),
        os.path.basename(self.unit.filename),
        self.sloc_range,
    )


for cls in [libfoolang.Decl, libfoolang.Using, libfoolang.Ref]:
    cls.__repr__ = Name_repr


def dump_xref(unit):
    for block in unit.root:
        print("In {}:".format(name_img(block)))
        for ref in block.f_refs:
            print("   {} resolves to {}".format(ref, ref.p_entity))


print("After first parsing:")
foo = load_unit("foo.txt")
bar = load_unit("bar.txt")
dump_xref(foo)
dump_xref(bar)

print("After reparsing foo:")
foo.reparse()
dump_xref(bar)

print("main.py: Done.")
