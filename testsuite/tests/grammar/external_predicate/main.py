import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()

for text in ["foo", "Foo"]:
    print(f"Parsing {text}:")
    u = ctx.get_from_buffer("main.txt", text)
    if u.diagnostics:
        for d in u.diagnostics:
            print("  ", d)
    else:
        print("   OK")
    print()

print("main.py: Done.")
