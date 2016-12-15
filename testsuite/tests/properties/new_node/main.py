print 'main.py: Running...'


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', '(main 1, 2, 3)')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
u.root.p_prop.dump()
print 'main.py: Done.'
