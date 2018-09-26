from __future__ import absolute_import, division, print_function

import libfoolang


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_file('foo.txt')
for node in unit.root.findall(lambda _: True):
    print('{} node: {}'.format(
        'Ghost' if node.is_ghost else 'Regular',
        node
    ))
    assert not node.text or not node.is_ghost

print('Done.')
