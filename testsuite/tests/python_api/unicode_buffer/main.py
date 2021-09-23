from collections import namedtuple

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext('iso-8859-1')
u = ctx.get_from_buffer('foo.txt', b'')


def get_from_buffer(buffer, charset):
    ctx.get_from_buffer('foo.txt', buffer, charset)


def reparse(buffer, charset):
    u.reparse(buffer, charset)


# Check that get_from_buffer/reparse correctly process buffer/charset
# arguments:
#
# * either buffer is a bytes string, then the charset argument (if provided) or
#   the context-wide charset (iso-8859-1, see above) is used to decode it.
#
# * either buffer is a Unicode string, then the charset argument must be null
#   (None or empty string).

Testcase = namedtuple('Testcase', 'buffer charset')

testcases = [
    Testcase(u'example # H\xe9llo', None),
    Testcase(u'example # H\xeallo', ''),
    Testcase(u'example # H\xebllo', 'utf-8'),

    Testcase(b'example # H\xe9llo', None),
    Testcase(b'example # H\xeallo', ''),
    Testcase(b'example # H\xebllo', 'iso-8859-1'),
    Testcase(b'example # H\xecllo', 'utf-8'),
    Testcase(b'example # H\xecllo', 'unknown-charset'),

    # Check that successfully parsing a unit with one encoding (UTF-8) has no
    # influence on the default encoding used later.
    Testcase(b'example # H\xc3\xa9llo', 'utf-8'),
    Testcase(b'example # H\xc3\xa9llo', None),
]


for method in (get_from_buffer, reparse):
    print('== {} =='.format(method.__name__))
    print('')
    for tc in testcases:
        try:
            method(tc.buffer, tc.charset)
        except Exception as exc:
            result = '{}: {}'.format(type(exc).__name__, exc)
        else:
            if u.diagnostics:
                result = '\n'.join(['diagnostics:'] +
                                   ['    {}'.format(d)
                                    for d in u.diagnostics])
            else:
                result = repr(u.text)

        print('  buffer={}, charset={}: {}'.format(
            repr(tc.buffer), repr(tc.charset), result,
        ))

print('main.py: Done.')
