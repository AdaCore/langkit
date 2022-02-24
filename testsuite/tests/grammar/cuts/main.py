import libfoolang


inputs = [
    ('valid case 1', "def a"),
    ('invalid case 1: use cut', "def a def"),
    ('valid case 2: use stopcut', "def a { def var b }"),
]

ctx = libfoolang.AnalysisContext()

for (name, text) in inputs:
    print(f"=== {name} ===")
    print()
    u = ctx.get_from_buffer(f"<{name}>", buffer=text)

    for d in u.diagnostics:
        print(d)
    u.root.dump()
    print()
