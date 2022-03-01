import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file('main.txt')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

for unit_arg in (u, None):
    print(f'id_unit({unit_arg}) = {u.root.p_id_unit(unit_arg)}')

print('result = {}'.format(u.root.p_result))

try:
    res_none = u.root.p_eval_unit(None)
except libfoolang.PropertyError as exc:
    res_none = '<PropertyError: {}>'.format(exc)
print('unit.root.eval_unit(None) = {}'.format(res_none))

try:
    res_int = u.root.p_eval_unit(42)
except TypeError as exc:
    res_int = '<TypeError: {}>'.format(exc)
print('unit.root.eval_unit(42) = {}'.format(res_int))

print('unit.root.eval_unit(unit) = {}'.format(u.root.p_eval_unit(u)))

print('main.py: Done.')
