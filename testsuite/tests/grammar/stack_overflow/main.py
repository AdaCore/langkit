import libfoolang


print("main.py: Running...")
print("")


def test(tree_depth):
    print("Parsing {} nested parens...".format(tree_depth))

    ctx = libfoolang.AnalysisContext()
    source = b"".join([
        b"(" * tree_depth,
        b"example",
        b")" * tree_depth,
    ])
    u = ctx.get_from_buffer("main.txt", source)

    if u.diagnostics:
        print("Diagnostics:")
        for d in u.diagnostics:
            print("   {}".format(d))
    else:
        print("No diagnostic")
    print("")


# The default recursion limit is 1000. There are two functions involved in
# parsing each () nesting level: the function for the "main_rule" parsing rule,
# and the one for the "paren" parsing rule, so in theory we can parse up to
# 499 nested parens (2 * 499 = 998 recursions) + the the example keyword (one
# recursion for the "main_rule" parsing rule and another for the "example"
# parsing rule).
test(499)
test(500)


print("main.py: Done.")
