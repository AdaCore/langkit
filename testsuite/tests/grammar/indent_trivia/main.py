import libfoolang


ctx = libfoolang.AnalysisContext()


for filename in ('foo1.txt', 'foo2.txt'):
    print('== {} =='.format(filename))
    u = ctx.get_from_file(filename)
    for token in u.iter_tokens():
        print('>>> {: <12} {: <10} {}'.format(
            token.kind, repr(token.text), token.sloc_range.start
        ))
    print('')
