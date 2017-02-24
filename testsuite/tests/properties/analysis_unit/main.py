from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
for f in ('main.txt', ):
    print('=== {} ==='.format(f))
    u = ctx.get_from_file(f)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)

    u.populate_lexical_env()
    print('result = {}'.format(u.root.p_result))

print('main.py: Done.')
