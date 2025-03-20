import sys

import libfoolang


ctx = libfoolang.AnalysisContext()

text = b"1 + 2"
u = ctx.get_from_buffer("main.txt", text)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print("Evaluating {}".format(repr(text)))
print("result = {}".format(u.root.p_result))
