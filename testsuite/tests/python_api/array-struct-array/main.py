from __future__ import absolute_import, division, print_function

import libfoolang as lfl


ctx = lfl.AnalysisContext()
u = ctx.get_from_buffer("test", "example")
print(u.root.p_example_holders)
