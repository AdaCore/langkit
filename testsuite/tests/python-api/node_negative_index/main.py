import libfoolang


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer('foo.txt', 'a b c')
for i in range(-4, 5):
    try:
        child = unit.root[i]
    except IndexError:
        child = '<IndexError>'
    print('i={}: {}'.format(i, child))
