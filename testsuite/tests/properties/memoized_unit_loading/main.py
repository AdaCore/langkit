from __future__ import absolute_import, division, print_function

import sys

import libfoolang


def pflush(msg):
    print(msg)
    sys.stdout.flush()


pflush('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        pflush(d)
    sys.exit(1)

for i in range(3):
    pflush('root.p_mmz_prop() = {}'.format(u.root.p_mmz_prop))

pflush('main.py: Done.')
