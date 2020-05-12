from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import emit_and_print_errors


for lkt_file in ('no-dot.lkt', 'unknown-node.lkt', 'unknown-prop.lkt'):
    print('== {} =='.format(lkt_file))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

        @langkit_property()
        def pred():
            return True

    emit_and_print_errors(lkt_file=lkt_file)
    print('')


print('Done')
