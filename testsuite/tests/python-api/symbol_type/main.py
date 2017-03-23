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

try:
    res_none = u.root.p_sym(None)
except TypeError as exc:
    res_none = '<TypeError: {}>'.format(exc)
print('u.root.p_sym(None) = {}'.format(res_none))

try:
    res_error = u.root.p_sym(42)
except TypeError as exc:
    res_error = '<TypeError: {}>'.format(exc)
print('u.root.p_sym(42) = {}'.format(res_error))

res_field = u.root.p_sym(u.root.f_tok.text)
print('u.root.p_sym(u.root.f_tok.symbol) = {}'.format(res_field))

res_no_such_symbol = u.root.p_sym('no_such_symbol')
print('u.root.p_sym("no_such_symbol") = {}'.format(res_no_such_symbol))

print('main.py: Done.')
