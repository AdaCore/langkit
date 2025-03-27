import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo.txt", b"my_ident")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for s in (
    None,
    42,
    "my_ident",
    "MY_IDENT",
    "no_such_symbol",
    "invalid_symbol0",
):
    try:
        result = "= {}".format(repr(u.root.p_sym(s)))
    except TypeError as exc:
        result = "raised <TypeError: {}>".format(exc)
    except libfoolang.InvalidSymbolError as exc:
        result = "raised <InvalidSymbolError: {}>".format(exc)
    print("u.root.p_sym({}) {}".format(repr(s), result))

print("main.py: Done.")
