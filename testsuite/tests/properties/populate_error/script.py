from __future__ import absolute_import, division, print_function

import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('source.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print('Calling AnalysisUnit.populate_lexical_env()...')
try:
    u.populate_lexical_env()
except libfoolang.PropertyError:
    print('Got a PropertyError as expected!')
else:
    print('Got no PropertyError, which is unexpected!')
print('Done.')
