from __future__ import absolute_import, division, print_function

import sys

import libfoolang


print('main.py: Running...')

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

print(".double(u'a') = {}".format(repr(n.p_double('a'))))
print(".double(u'\\u03c0') = {}".format(repr(n.p_double(u'\u03c0'))))

for obj in (1, 'foo', ['a', 'b'], u'h\xe9llo', 'h\xe9llo', ['a', 1]):
    try:
        value = n.p_text_identity(obj)
    except Exception as exc:
        value = '<{}: {}>'.format(type(exc).__name__, exc)
    else:
        value = repr(value)
    print('.text_identity({}) = {}'.format(repr(obj), value))

print('main.py: Done.')
