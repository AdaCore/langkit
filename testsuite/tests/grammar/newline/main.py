import libfoolang


ctx = libfoolang.AnalysisContext()


def process(text):
    u = ctx.get_from_buffer("main.txt", text)
    if u.diagnostics:
        print("Found errors:")
        for d in u.diagnostics:
            print(" {}".format(d))
    else:
        u.root.dump()


process(
    b"""
1
2
""".strip()
)
process(b"1 2")
process(
    b"""
1
   2
""".strip()
)

process(
    b"""
1
2
   3
4
""".strip()
)
