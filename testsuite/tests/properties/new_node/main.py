import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", b"(main 1, 2, 3)")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def check(prop, **kwargs):
    kwargs_fmt = ", ".join(f"{k}={v}" for k, v in kwargs.items())
    print(f"== {prop}({kwargs_fmt}) ==")
    try:
        prop_value = getattr(u.root, f"p_{prop}")
        node = prop_value(**kwargs) if kwargs else prop_value
    except libfoolang.PropertyError as exc:
        print(f"PropertyError: {exc}")
    else:
        node.dump()
    print("")


for prop in ("prop", "prop2"):
    for with_null in (False, True):
        check(prop, with_null=with_null)
check("prop3")

print("main.py: Done.")
