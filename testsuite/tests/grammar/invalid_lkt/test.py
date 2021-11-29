"""
Test that invalid uses of abstract fields are duly diagnosed and rejected.
"""

import glob

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


for lkt_file in sorted(glob.glob('*.lkt')):

    # Skip the source that contains common declarations for all tests
    if lkt_file == "nodes.lkt":
        continue

    print('== {} =='.format(lkt_file))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file=lkt_file, lkt_semantic_checks=True)
    print('')

print('Done')
