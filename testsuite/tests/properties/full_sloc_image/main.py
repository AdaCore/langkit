from __future__ import absolute_import, division, print_function

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', 'example')
print(libfoolang._py2to3.text_repr(u.root.full_sloc_image))
