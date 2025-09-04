import liblktlang as lkt


ctx = lkt.AnalysisContext()
u = ctx.get_from_file("test.lkt")

refs = u.root.findall(lkt.RefId)
dots = u.root.findall(lkt.DotExpr)


def complete_kind(name: lkt.DefId) -> str:
    return lkt.CompletionItemKind._c_to_py[name.p_completion_item_kind - 1]


for d in dots:
    print(f"Completing {d} ('{d.text}')")
    for ci in d.p_complete:
        if ci.declaration.f_syn_name is not None:
            print(
                f" - {ci.declaration.p_full_name}:"
                f" {complete_kind(ci.declaration.f_syn_name)}"
            )
ref = refs[-2:]
for r in ref:
    print(f"Completing {r} ('{r.text}')")
    for ci in r.p_complete:
        if ci.declaration.f_syn_name is not None:
            print(
                f" - {ci.declaration.p_full_name}:"
                f" {complete_kind(ci.declaration.f_syn_name)}"
            )
