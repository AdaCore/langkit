"""
Check that diagnostics emitted during parsing are correctly discarded when the
"branch" responsible for them is discarded itself.
"""

import libfoolang


inputs = [
    ("or_rule", "or alt b;"),
    ("opt_rule", "opt id;"),
    ("list_rule", "list item id=; item end"),
]

ctx = libfoolang.AnalysisContext()

for name, text in inputs:
    print(f"== {name} ==")
    print()
    u = ctx.get_from_buffer("buffer", buffer=text)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        print()
    u.root.dump()
    print()
