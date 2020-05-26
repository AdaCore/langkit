import sys

import libfoolang
from libfoolang import _py2to3


ctx = libfoolang.AnalysisContext()

text = b'1 + 2'
u = ctx.get_from_buffer('main.txt', text)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

print('Evaluating {}'.format(_py2to3.bytes_repr(text)))
print('result = {}'.format(u.root.p_result))
