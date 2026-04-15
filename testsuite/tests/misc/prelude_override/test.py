import liblktlang as lkt


ctx = lkt.AnalysisContext()

ctx.get_from_buffer("__prelude", "@builtin struct Int {}")

unit = ctx.get_from_buffer("foo.lkt", "val x: Int = 1")

for d in unit.root.findall(lkt.ValDecl):
    if d.p_xref_entry_point:
        results = d.p_solve_enclosing_context
        print(results)
