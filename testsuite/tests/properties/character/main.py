from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', 'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root

print(".get_a() = {}".format(repr(n.p_get_a())))
print(".get_eacute() = {}".format(repr(n.p_get_eacute())))
print(".identity(u'a') = {}".format(repr(n.p_identity('a'))))
print(".identity(u'\\u03c0') = {}".format(repr(n.p_identity(u'\u03c0'))))

for obj in (1, 'aa', u'aa'):
    print('Trying to evaluate .identity({})'.format(repr(obj)))
    try:
        n.p_identity(obj)
    except Exception as exc:
        print('   ... got a {}: {}'.format(type(exc).__name__, exc))
    else:
        print('   ... got no exception')

print('main.py: Done.')
