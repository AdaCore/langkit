from __future__ import absolute_import, division, print_function

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "foo.py", """
def test(a, b):
    return a - b
    raise Exception
"""
)
r = u.root.find(libfoolang.RaiseStmt)
print("{}{}".format(r.full_sloc_image, "raise statement"))
