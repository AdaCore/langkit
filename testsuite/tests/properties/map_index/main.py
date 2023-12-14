import sys

import libfoolang


print("main.py: Running...")
print("")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"1 2 3 4")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def print_item(item):
    if isinstance(item, libfoolang.IndexedNumber):
        print(f"{item.index}: NumberNode({item.number.text})")
    else:
        assert isinstance(item, libfoolang.NumberNode)
        print(f"NumberNode({item.text})")


for prop in (
    "p_map_no_idx",
    "p_map_idx",
    "p_mapcat_no_idx",
    "p_mapcat_idx",
    "p_filter_no_idx",
    "p_filter_idx",
    "p_filtermap_no_idx",
    "p_filtermap_elt_idx",
    "p_filtermap_filter_idx",
    "p_filtermap_all_idx",
    "p_take_while_no_idx",
    "p_take_while_idx",
):
    print(f"== {prop} ==")
    for item in getattr(u.root, prop):
        print_item(item)
    print("")

print("main.py: Done.")
