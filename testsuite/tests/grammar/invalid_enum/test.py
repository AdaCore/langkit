from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


for lkt_file in ('no-dot.lkt', 'bad-prefix.lkt', 'bad-alt.lkt'):
    print('== {} =='.format(lkt_file))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        enum_node = True
        alternatives = ['example', 'null', 'def']

    emit_and_print_errors(lkt_file=lkt_file)
    print('')


print('Done')
