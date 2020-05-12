import libfoolang as lfl


ctx = lfl.AnalysisContext()
u = ctx.get_from_buffer("test", "example")
print(u.root.p_example_holders)
