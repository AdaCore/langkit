import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo.txt", b"example")
print("Calling FooNode.p_prop...")
try:
    print("  ->", u.root.p_prop)
except Exception as exc:
    print("  Got an exception:")
    print(f"    {type(exc).__name__}: {exc}")

print("main.py: Done.")
