import libfoolang

import sys


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b"e " * 1500000)

if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print('main.py: Populating lexical envs...')

u.populate_lexical_env()

print('main.py: Done')
