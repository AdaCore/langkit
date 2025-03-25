import sys

import libfoolang


print("main.py: Running...")
print()

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    b"""
    ()
    (a0)
    (b0 b1 b2)
    """,
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def img(value):
    if value is None:
        return "None"
    elif isinstance(value, list):
        return "[{}]".format(". ".join(img(v) for v in value))
    elif isinstance(value, (libfoolang.Tuple, libfoolang.Name)):
        return value.text
    else:
        return img(value.parent)


# Compute references to the 0-element, 1-element and 3-elements collections
# (`n*` for list nodes, and `a*` for arrays).
n0, n1, n3 = u.root
a0 = list(n0.f_items)
a1 = list(n1.f_items)
a3 = list(n3.f_items)

# We want to test all properties passing to them the kind of collection they
# expect.
for kind, (c0, c1, c3) in [
    ("list", (n0.f_items, n1.f_items, n3.f_items)),
    ("array", (a0, a1, a3)),
]:
    for prop_name in [
        f"p_{kind}_subscript",
        f"p_{kind}_at",
        f"p_{kind}_nc_subscript",
        f"p_{kind}_nc_at",
    ]:
        prop = getattr(u.root, prop_name)

        # Now proceed to call them with the various combinations of arguments
        print("#", prop_name)
        print()

        test_items = []
        if kind == "list":
            # Pass None both to the collection and to the index getter to check
            # the null-cond shortcut behavior (or absence thereof).
            test_items += [
                (None, None),
                (None, n0),
                (n0.f_items, None),
            ]

        # One out-of-bounds access, one in-bounds
        test_items += [(c1, n3), (c3, n1)]

        for items, index_getter in test_items:
            if index_getter is None:
                index_img = "None"
            else:
                index_img = str(len(index_getter))

            try:
                result = prop(items, index_getter)
            except libfoolang.PropertyError as exc:
                result_img = f"<{type(exc).__name__}: {exc}>"
            else:
                result_img = img(result)

            print(f"items={img(items)}, index={index_img}:")
            print(f"  {result_img}")

        print()

print("main.py: Done.")
