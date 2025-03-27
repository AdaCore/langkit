import sys

import libfoolang


print("main.py: Running...")

u = libfoolang.AnalysisContext().get_from_buffer("main.txt", b"example")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

e = u.root[0]
s1 = e.p_get_struct
s2 = e.p_struct_identity(
    libfoolang.MyStruct(
        entity_field=s1.entity_field, array_field=[], bigint_field=2
    )
)

for name, s in [("First struct", s1), ("Second struct", s2)]:
    print("{}:".format(name))
    print("s.entity_field = {}".format(s.entity_field))
    print("s.array_field = {}".format(s.array_field))
    print("s.bigint_field = {}".format(s.bigint_field))
    print("")

print("main.py: Done.")
print("")
