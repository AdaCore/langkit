from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'a b c')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()

empty = ('LexicalEnv.empty', libfoolang.LexicalEnv.Empty)
non_empty = ('u.root.children_env', u.root.children_env)

for (arg1_str, arg1), (arg2_str, arg2) in [
    (empty, empty),
    (empty, non_empty),
    (non_empty, empty),
    (non_empty, non_empty),
]:
    try:
        result = u.root.p_is_visible_from(arg1, arg2)
    except libfoolang.PropertyError as exc:
        result = '<PropertyError: {}>'.format(exc)
    print('u.root.p_is_visible_from({}, {}) = {}'.format(
        arg1_str, arg2_str, result
    ))

print('main.py: Done.')
