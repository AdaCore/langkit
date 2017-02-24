from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import libfoolang

ctx = libfoolang.AnalysisContext()


def process(text):
    u = ctx.get_from_buffer('main.txt', text)
    if u.diagnostics:
        print("Found errors:")
        for d in u.diagnostics:
            print("", d)
    else:
        u.root.dump()


process("""
1
2
""".strip())
process("1 2")
process("""
1
   2
""".strip())

process("""
1
2
   3
4
""".strip())
