import sys

import libfoolang
from libfoolang import _py2to3


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('main.txt', b'example')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

n = u.root


def unirepr(value):
    if isinstance(value, _py2to3.text_type):
        return _py2to3.text_repr(value)
    elif isinstance(value, _py2to3.bytes_type):
        return _py2to3.bytes_repr(value)
    elif isinstance(value, list):
        return '[{}]'.format(', '.join(unirepr(item) for item in value))
    else:
        return repr(value)


print('.get_a() = {}'.format(unirepr(n.p_get_a())))
print('.get_eacute() = {}'.format(unirepr(n.p_get_eacute())))
for arg in (b'a', u'\u03c0'):
    print('.identity({}) = {}'.format(unirepr(arg),
                                      unirepr(n.p_identity(arg))))

for obj in (1, b'aa', u'aa'):
    print('Trying to evaluate .identity({})'.format(unirepr(obj)))
    try:
        n.p_identity(obj)
    except Exception as exc:
        print('   ... got a {}: {}'.format(type(exc).__name__, exc))
    else:
        print('   ... got no exception')

print(".double(u'a') = {}".format(unirepr(n.p_double('a'))))
print(".double(u'\\u03c0') = {}".format(unirepr(n.p_double(u'\u03c0'))))

for obj in (1, b'foo', [b'a', b'b'], u'h\xe9llo', b'h\xe9llo', [b'a', 1]):
    try:
        value = n.p_text_identity(obj)
    except Exception as exc:
        value = '<{}: {}>'.format(type(exc).__name__, exc)
    else:
        value = unirepr(value)
    print('.text_identity({}) = {}'.format(unirepr(obj), value))

print('main.py: Done.')
