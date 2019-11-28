from __future__ import absolute_import, division, print_function

import sys


def pflush(msg):
    print(msg)
    sys.stdout.flush()

pflush('main.py: Running...')


import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'a(c) b(a c) +c(a)')
if u.diagnostics:
    for d in u.diagnostics:
        pflush(d)
    sys.exit(1)

u.populate_lexical_env()

pflush('Calling p_entity_items...')
u.root[0].p_entity_items

pflush('main.py: Done.')
