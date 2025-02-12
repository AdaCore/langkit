import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("example", "example")
print(u.root.p_main)
print("main.py: Done.")
