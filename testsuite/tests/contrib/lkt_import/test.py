"""
Test that Lkt's import statements work as expected.
"""

from __future__ import absolute_import, division, print_function

from collections import namedtuple
import os

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


Test = namedtuple('Test', 'label lkt_file path abspath')


for t in [
    Test('Unit not found', 'other-dir.lkt', [], abspath=False),
    Test('Unit in path, relative', 'other-dir.lkt', ['src'], abspath=False),
    Test('Unit in path, absolute', 'other-dir.lkt', ['src'], abspath=True),
    Test('Import loop', 'loop.lkt', [], False),
]:
    print('== {} =='.format(t.label))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

    os.environ['LKT_PATH'] = ':'.join((os.path.abspath(d) if t.abspath else d)
                                      for d in t.path)
    emit_and_print_errors(lkt_file=t.lkt_file)
    print('')

print('Done')
