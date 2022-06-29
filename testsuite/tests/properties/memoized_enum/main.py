import sys

import libfoolang


print('main.py: Running...')


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer("main.txt", "example")
if unit.diagnostics:
    for d in unit.diagnostics:
        print(d)
    sys.exit(1)

for label in ("First round", "Second round"):
    print(f"== {label} ==")
    for prop, values in [
        ("p_id_bool", [False, True]),
        ("p_id_my_enum", [libfoolang.MyEnum.a, libfoolang.MyEnum.b]),
    ]:
        for v in values:
            print(f"{prop}({v}) =", getattr(unit.root, prop)(v))
    print()

print('main.py: Done.')
