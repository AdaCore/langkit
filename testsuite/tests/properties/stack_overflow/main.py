import libfoolang


print('main.py: Running...')
print('')

max_depth = 1000
ctx = libfoolang.AnalysisContext()


def parse(source):
    u = ctx.get_from_buffer('main.txt', source)
    if u.diagnostics:
        print('Diagnostics:')
        for d in u.diagnostics:
            print('  ', d)
    return u.root


def test(label, test_func):
    print('== {} =='.format(label))
    try:
        result = test_func()
    except libfoolang.PropertyError as exc:
        print('PropertyError raised: {}'.format(str(exc).strip()))
    else:
        print('Returned: {}'.format(result))
    print('')


n = parse(b'example')
test('No overflow: property calls', lambda: n.p_recurse(max_depth))
test('Overflow: property calls', lambda: n.p_recurse(max_depth + 1))

print('main.py: Done.')
