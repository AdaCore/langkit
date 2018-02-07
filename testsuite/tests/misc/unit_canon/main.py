from __future__ import absolute_import, division, print_function

import itertools
import os.path

import libfoolang


ctx = libfoolang.AnalysisContext()

filenames = [
    ('rel_foo', 'foo.txt'),
    ('abs_foo', os.path.abspath('foo.txt')),
    ('rel_bar', 'bar'),
]

for (label1, f1), (label2, f2) in itertools.product(filenames, filenames):
    u1 = ctx.get_from_file(f1)
    u2 = ctx.get_from_file(f2)
    print('Comparing root nodes for {} and {}: {}'.format(
        label1, label2,
        u1.root == u2.root
    ))

print('Done.')
