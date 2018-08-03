from __future__ import absolute_import, division, print_function

import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer('foo.txt', 'example (example)')

# Make sure that the exact context wraper is re-used over and over
assert unit.context is ctx

if unit.diagnostics:
    for d in unit.diagnostics:
        print(d)
    sys.exit(1)

root = unit.root
child = root[0]

# Make sure that the exact unit/node wrapper is re-used over and over
assert child.parent is root
assert root[0] is child
assert root.unit is unit

print('main.py: Done.')
