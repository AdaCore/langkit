import os.path

import libfoolang


ctx = libfoolang.AnalysisContext()

f1, f2, f3 = [
    ("rel_foo", "foo.txt"),
    ("abs_foo", os.path.abspath("foo.txt")),
    ("rel_bar", "bar.txt"),
]

tests = [
    (f1, f1, True),
    (f1, f2, True),
    (f1, f3, False),
    (f2, f1, True),
    (f2, f2, True),
    (f2, f3, False),
    (f3, f1, False),
    (f3, f2, False),
    (f3, f3, True),
]

for (label1, f1), (label2, f2), exp_result in tests:
    u1 = ctx.get_from_file(f1)
    u2 = ctx.get_from_file(f2)
    result = u1.root == u2.root
    print(
        "Comparing root nodes for {} and {}: {}".format(label1, label2, result)
    )
    if result != exp_result:
        print("   u1 = {}".format(u1.filename))
        print("   u2 = {}".format(u2.filename))

        print("   n1:tuple = {}".format(u1.root._id_tuple))
        print("   n1:env_rebindings = {}".format(u1.root._rebindings))

        print("   n2:tuple = {}".format(u2.root._id_tuple))
        print("   n2:env_rebindings = {}".format(u2.root._rebindings))

print("Done.")
