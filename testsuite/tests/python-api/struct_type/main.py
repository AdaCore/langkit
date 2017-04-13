from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', 'my_ident')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
thing = libfoolang.Thing(f_node=u.root, f_comes_from_source=True)
try:
    res_none = u.root.p_entity_id(None)
except Exception as exc:
    res_none = '<{}: {}>'.format(type(exc).__name__, exc)
thing = u.root.p_entity_id(thing)
print('u.root.p_entity(None) = {}'.format(res_none))
print('u.root.p_entity(entity) = {}'.format(thing))
print('main.py: Done.')
