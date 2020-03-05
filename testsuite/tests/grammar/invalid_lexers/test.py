from __future__ import absolute_import, division, print_function

import glob

import langkit
from langkit.dsl import ASTNode

from utils import emit_and_print_errors


for lkt_file in sorted(glob.glob('*.lkt')):
    print('== {} =='.format(lkt_file))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file=lkt_file)
    langkit.reset()
    print('')

print('Done')
