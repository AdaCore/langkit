import liblktlang as lkt


ctx = lkt.AnalysisContext()
common = ctx.get_from_file("common.lkt")
unit = ctx.get_from_file("test.lkt")


def print_impls(decl: lkt.FunDecl):
    print(f"{decl} implemented at:")
    for ref in decl.p_find_all_overrides([common, unit]):
        print(f" - {ref}")
    print()


# Test all declarations of test.lkt
for decl in unit.root.findall(lkt.FunDecl):
    print_impls(decl.cast(lkt.FunDecl))

# Test all declarations of test.lkt
for decl in common.root.findall(lkt.FunDecl):
    print_impls(decl.cast(lkt.FunDecl))
