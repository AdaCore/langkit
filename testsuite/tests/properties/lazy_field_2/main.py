import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


r = u.root
assert not r.p_is_flag_enabled
assert not r.f_my_field.p_is_flag_enabled

rf = r.p_with_flag(True)
assert rf.p_is_flag_enabled
assert rf.f_my_field.p_is_flag_enabled

print('main.py: Done.')
