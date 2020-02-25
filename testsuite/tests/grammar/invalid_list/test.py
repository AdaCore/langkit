from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Annotations, Field

from utils import emit_and_print_errors


def run(label, lkt_file):
    print('== {} =='.format(label))

    class FooNode(ASTNode):
        annotations = Annotations(generic_list_type='FooList')

    class ListNode(FooNode):
        items = Field()

    class Example(FooNode):
        token_node = True

    class Num(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file=lkt_file)
    print('')


run('Token element', 'token-element.lkt')
run('Non-list list_cls', 'non-list-cls.lkt')
run('Invalid element type in list_cls', 'invalid-element-type.lkt')
print('Done')
