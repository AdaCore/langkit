import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    b"""
def a
    (1 + (2 + c))
def b
    ((10 + (20 + a)) + 30)
def c
    100
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for d in u.root:
    for prop in (
        "p_evaluate_abstract",
        "p_evaluate_rtcheck",
        "p_evaluate_concrete",
        "p_evaluate_entity",
    ):
        try:
            result = getattr(d.f_expr, prop)
        except libfoolang.PropertyError as exc:
            result = str(exc).strip()
            result = "<{}: {}>".format(type(exc).__name__, exc)
        print("{}.{} = {}".format(d.f_name.text, prop, result))
    print("")

print("main.py: Done.")
