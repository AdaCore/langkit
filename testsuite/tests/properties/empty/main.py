import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"() (example) (example example example)")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


for seq in u.root:
    print(f"check_list({seq.text}) = {seq.p_check_list(seq.f_items)}")
print("")

for ints in [[], [1], [1, 2, 3]]:
    print(f"check_array({ints}) = {u.root.p_check_array(ints)}")
print("")

print("main.py: Done.")
