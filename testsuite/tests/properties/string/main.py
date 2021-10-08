import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file("main.txt")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root


for obj in (1, b"foo", [b"a", b"b"], u"h\xe9llo", b"h\xe9llo", [b"a", 1]):
    try:
        value = n.p_identity(obj)
    except Exception as exc:
        value = "<{}: {}>".format(type(exc).__name__, exc)
    else:
        value = ascii(value)
    print(".identity({}) = {}".format(ascii(obj), value))

print(".extend({}) = {}".format(ascii("bar"), ascii(n.p_extend("bar"))))
print(".newline() = {}".format(ascii(n.p_newline)))

print("main.py: Done.")
