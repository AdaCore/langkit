import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"a(c) b(a c) +c(a)")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def nimage(node):
    return "{}({})".format(type(node).__name__, node.f_name.text)


def eimage(entity):
    return "{}{}".format("+" if entity.p_b_set else "", nimage(entity))


libfoolang._trace = True
for decl in u.root:
    print(
        "{}{}:".format(
            "+" if decl.f_has_plus.p_as_bool else "", decl.f_name.text
        )
    )
    for ref in decl.f_items:
        ref_decl = ref.p_decl
        print("  {} -> {}".format(nimage(ref), eimage(ref_decl)))
        for sub_ref in ref_decl.p_entity_items:
            print("    {}".format(eimage(sub_ref)))

print("main.py: Done.")
