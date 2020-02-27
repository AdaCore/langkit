"""
Check that invalid token gramma rules are properly reported.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


for lkt_file in ('bad-token-casing.lkt', 'unknown-token.lkt'):
    print('== {} =='.format(lkt_file))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        pass

    emit_and_print_errors(lkt_file=lkt_file)
    print('')

print('Done')
