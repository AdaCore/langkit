import libfoolang


ctx = libfoolang.AnalysisContext()


def run_tests(section, inputs):
    print("=" * 79)
    print("=", section)
    print("=" * 79)
    print()
    for name, text in inputs:
        print(f"=== {name} ===")
        print()
        u = ctx.get_from_buffer("buffer", buffer=text)

        if u.diagnostics:
            for d in u.diagnostics:
                print(d)
            print()

        if u.root is None:
            print("<no tree>")
        else:
            u.root.dump()
        print()


run_tests(
    "stop_cut",
    [
        ("valid case 1", "stop_cut def a"),
        ("invalid case 1: use cut", "stop_cut def a def"),
        ("valid case 2: use stopcut", "stop_cut def a { def var b }"),
    ],
)
run_tests(
    "dont_skip",
    [
        ("valid", "dont_skip { def a def b }"),
        ("missing }", "dont_skip { def a def b"),
        ("missing keyword", "dont_skip { def a b }"),
        ("missing id", "dont_skip { def a def }"),
    ],
)
run_tests(
    "dont_skip_non_root",
    [
        ("valid", "dont_skip_non_root { def a def b }"),
        ("missing id", "dont_skip_non_root { def a def }"),
    ],
)
