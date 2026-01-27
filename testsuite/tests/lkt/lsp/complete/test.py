import liblktlang as lkt


ctx = lkt.AnalysisContext()
u = ctx.get_from_file("test.lkt")

refs = u.root.findall(lkt.RefId)
dots = u.root.findall(lkt.DotExpr)


def complete_kind(name: lkt.DefId) -> str:
    return lkt.CompletionItemKind._c_to_py[name.p_completion_item_kind - 1]


def sorted_complete(n: lkt.LktNode) -> list[lkt.Decl]:
    result = [
        ci.declaration
        for ci in n.p_complete
        if ci.declaration.f_syn_name is not None
    ]
    result.sort(key=lambda d: d.p_full_name)
    return result


for n in dots + refs[-2:]:
    print(f"Completing {n} ('{n.text}')")
    for d in sorted_complete(n):
        print(f" - {d.p_full_name}: {complete_kind(d.f_syn_name)}")
