import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("foo.txt", b"example")
print("===")
print(u.root.dump_str())
print("===")
print("main.py: Done.")
