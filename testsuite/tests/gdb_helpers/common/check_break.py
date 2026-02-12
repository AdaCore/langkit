"""
Checks for the breakpoint commmand.
"""

from helpers import run_continue, run_foobreak, start_gdb


gdb = start_gdb("break")

# Check that breaking on the properties in an overriding property tree works as
# expected.
for node_break, img, base in [
    ("Parens", "<Parens main.txt:1:1-1:18>", "10"),
    ("Example", "<Example main.txt:1:19-1:25>", "20"),
    ("FooNode", "<FooNodeList main.txt:1:1-1:25>", "30"),
]:
    prop = f"{node_break}.overriding_prop"
    run_foobreak(prop)
    run_continue()
    gdb.test(
        "foostate",
        "\n".join(
            [
                f"Running {prop}",
                "  from @...test.lkt:@NUMBER",
                f"  node = {img}",
                f"  base = {base}",
            ]
        ),
    )
