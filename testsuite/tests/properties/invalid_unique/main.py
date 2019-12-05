from __future__ import absolute_import, division, print_function

import os.path

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
a, b, c, d = [ctx.get_from_buffer('{}.txt'.format(name), b'example')
              for name in ('a', 'b', 'c', 'd')]
node = a.root


def fmt(array):
    return '[{}]'.format(', '.join(os.path.basename(u.filename)
                                   for u in array))


for array in [
    [],
    [a, b, c, d],
    [d, c, b, a],
    [a, a, b, a, b, c, d, d]
]:
    print('{}.unique = {}'.format(fmt(array), fmt(node.p_test(array))))

print('main.py: Done.')
