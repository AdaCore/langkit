"""
Test that invalid uses of abstract fields are duly diagnosed and rejected.
"""

from __future__ import absolute_import, division, print_function

import glob

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


for i, lkt_file in enumerate(sorted(glob.glob('*.lkt'))):
    print('== {} =='.format(lkt_file))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file=lkt_file)
    print('')

print('Done')
