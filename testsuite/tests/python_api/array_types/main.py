import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', b'example null example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

try:
    res_none = u.root.p_count(None)
except TypeError as exc:
    res_none = '<TypeError: {}>'.format(exc)
print('u.root.p_count(None) = {}'.format(res_none))

try:
    res_int = u.root.p_count(42)
except TypeError as exc:
    res_int = '<TypeError: {}>'.format(exc)
print('u.root.p_count(42) = {}'.format(res_int))

try:
    res_bad_array = u.root.p_count(u.root.p_all_items)
except TypeError as exc:
    res_bad_array = '<TypeError: {}>'.format(exc)
print('u.root.p_count(u.root.p_all_items) = {}'.format(res_bad_array))

example_items = u.root.p_example_items
print('u.root.p_example_items = {}'.format(example_items))

res_array = u.root.p_count(example_items)
print('u.root.p_count(u.root.p_example_items) = {}'.format(res_array))

print('main.py: Done.')
