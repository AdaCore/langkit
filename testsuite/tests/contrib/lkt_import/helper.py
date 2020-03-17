"""
Test that Lkt's import statements work as expected.
"""

from __future__ import absolute_import, division, print_function

import sys

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


label = sys.argv[1]
lkt_file = sys.argv[2]


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


print('== {} =='.format(label))
emit_and_print_errors(lkt_file=lkt_file)
print('... done')
print('')
