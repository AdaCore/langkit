"""
Check that logic equations are parsed as expected.
"""

import glob

import liblktlang as lkt


def dump(n, indent=""):
    if n.is_token_node:
        print(f"{indent}{n.kind_name}: {n.text}")
    else:
        print(f"{indent}{n.kind_name}")
        child_indent = indent + "|   "
        if n.is_list_type:
            for child in n:
                dump(child, child_indent)
            if len(n) == 0:
                print(f"{child_indent}<empty>")
        else:
            for attr, child in n.iter_fields():
                if child is not None:
                    print(f"{indent}| {attr}:")
                    dump(child, child_indent)


ctx = lkt.AnalysisContext()

for filename in sorted(glob.glob("*.lkt")):
    print(f"== {filename} ==")
    print("")
    u = ctx.get_from_file(filename)
    if u.diagnostics:
        for d in u.diagnostics:
            print(u.format_gnu_diagnostic(d))
        raise RuntimeError
    else:
        for decl in u.root.findall(lkt.ValDecl):
            print(f"{decl.f_syn_name.text}:")
            dump(decl.f_val)
            print("")

print("Done")
