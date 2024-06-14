import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()

u = ctx.get_from_buffer("main.txt", "example")

props = [
    ("p_test_variadic", u.root.p_test_variadic),
    ("p_test_static", u.root.p_test_static)
]
for name, prop in props:
    for n in [3, 4, 0]:
        try:
            print(f"{name}({n}) = {prop(n)}")
        except libfoolang.PropertyError as e:
            print(f"{name}({n}) raised exception: {e}")

print('main.py: Done.')
