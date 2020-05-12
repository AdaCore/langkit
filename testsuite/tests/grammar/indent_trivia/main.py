import libfoolang
from libfoolang import _py2to3


ctx = libfoolang.AnalysisContext()


for filename in ('foo1.txt', 'foo2.txt'):
    print('== {} =='.format(filename))
    u = ctx.get_from_file(filename)
    for token in u.iter_tokens():
        print('>>> {: <12} {: <10} {}'.format(
            token.kind, _py2to3.text_repr(token.text), token.sloc_range.start
        ))
    print('')
