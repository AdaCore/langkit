import liblktlang as lkt


ctx = lkt.AnalysisContext()
u = ctx.get_from_file("test.lkt")

refs = u.root.findall(lkt.RefId)
dots = u.root.findall(lkt.DotExpr)


# Map integers returned by the p_completion_item_kind property to the
# corresponding CompletionItemKind enumeration values.
completion_kind_map = {
    i + 1: value for i, value in enumerate(lkt.CompletionItemKind)
}


def complete_kind(name: lkt.DefId) -> str:
    return completion_kind_map[name.p_completion_item_kind]


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
