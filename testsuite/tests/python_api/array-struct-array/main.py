import libfoolang as lfl
from libfoolang import ExampleHolder as EH


ctx = lfl.AnalysisContext()
u = ctx.get_from_buffer("test", "example")
n = u.root
print(f"p_example_holder = {n.p_example_holders}")
for a in [
    [],
    [EH([])],
    [EH([n])],
    [EH([n, n])],
    [EH([n, n]), EH([n]), EH([])],
]:
    print(f"p_identity({a}) = {u.root.p_identity(a)}")
