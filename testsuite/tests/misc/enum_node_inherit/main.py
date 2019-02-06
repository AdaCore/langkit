from __future__ import absolute_import, division, print_function

import libfoolang


ctx = libfoolang.AnalysisContext()
foo = ctx.get_from_file('foo.txt')

assert foo.root.is_a(libfoolang.HasExamplePresent)

print(foo.root.p_prop)

print('Done.')
