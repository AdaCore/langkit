import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"n1.n2.n3")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def image(n):
    return "None" if n is None else f"<{n.text}>"


n3 = u.root
n2 = n3.f_prefix
n1 = n2.f_prefix
for prop_name in [
    "p_field_1",
    "p_field_2",
    "p_field_3",
    "p_field_4",
    "p_call_1",
    "p_call_2",
    "p_call_3",
    "p_call_4",
]:
    prop = getattr(u.root, prop_name)

    for n in (None, n1, n2, n3):
        try:
            result = prop(n)
        except libfoolang.PropertyError as exc:
            result_img = f"<{type(exc).__name__}: {exc}"
        else:
            result_img = image(result)
        print(f"{prop_name}({image(n)}) = {result_img}")
    print("")

print("main.py: Done.")
