import liblktlang as lkt


ctx = lkt.AnalysisContext()
common = ctx.get_from_file("common.lkt")
unit = ctx.get_from_file("test.lkt")


def print_details(decl: lkt.Decl):
    print(f"{decl}:")
    print(f" - {decl.f_syn_name.p_decl_detail}")
    print(f" - {decl.f_syn_name.p_doc or '<no doc>'}")


# Test all declarations of test.lkt
for decl in unit.root.findall(lkt.Decl):
    print_details(decl.cast(lkt.Decl))

# Test all declarations of test.lkt
for decl in common.root.findall(lkt.Decl):
    print_details(decl.cast(lkt.Decl))
