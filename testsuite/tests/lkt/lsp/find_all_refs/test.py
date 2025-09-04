import liblktlang as lkt


ctx = lkt.AnalysisContext()
common = ctx.get_from_file("common.lkt")
unit = ctx.get_from_file("test.lkt")


def print_refs(decl: lkt.Decl):
    print(f"{decl} referenced by:")
    for ref in decl.f_syn_name.p_find_all_references([common, unit]):
        print(f" - {ref}")
    print()


# Test all declarations of test.lkt
for decl in unit.root.findall(lkt.Decl):
    print_refs(decl.cast(lkt.Decl))

# Test the root node to verify that it works across all given units
print_refs(common.root.find(lkt.ClassDecl).cast(lkt.Decl))
