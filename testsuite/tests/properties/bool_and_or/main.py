import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

root = u.root


def image(v):
    if v is True:
        return "T"
    elif v is False:
        return "F"
    elif v is None:
        return "<none>"
    else:
        assert v == root
        return "<root>"


for prop_name, b1, b2, n in [
    ("p_check_and", True, False, None),
    ("p_check_and", False, False, None),
    ("p_check_and", True, False, root),
    ("p_check_and", True, True, root),
    ("p_check_or", False, False, root),
    ("p_check_or", False, True, root),
    ("p_check_or", True, False, None),
]:
    prop = getattr(root, prop_name)
    try:
        result = prop(b1, b2, n)
    except libfoolang.PropertyError as exc:
        result_image = f"<{type(exc).__name__}: {exc}"
    else:
        result_image = image(result)
    print(
        f"{prop_name}({image(b1)}, {image(b2)}, {image(n)}) = {result_image}"
    )

print('main.py: Done.')
