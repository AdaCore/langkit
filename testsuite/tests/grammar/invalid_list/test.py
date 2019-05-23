from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Annotations, Field
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import emit_and_print_errors


def run(label, **kwargs):
    print('== {} =='.format(label))

    class FooNode(ASTNode):
        annotations = Annotations(generic_list_type='FooList')

    class ListNode(FooNode):
        items = Field()

    class Example(FooNode):
        token_node = True

    class Num(FooNode):
        token_node = True

    class T:
        pass
    T.FooNode = FooNode
    T.ListNode = ListNode
    T.Example = Example
    T.Num = Num

    g = Grammar('main_rule')
    g.add_rules(**{name: parser(T, g) for name, parser in kwargs.items()})
    emit_and_print_errors(g)
    print('')


run('Token element',
    main_rule=lambda T, _: T.ListNode(List(Token.Number)))

run('Non-list list_cls',
    num=lambda T, _: T.Num(Token.Number),
    main_rule=lambda T, g: List(g.num, list_cls=T.ListNode))

run('Invalid element type in list_cls',
    example=lambda T, _: T.Example(Token.Example),
    num=lambda T, _: T.Num(Token.Number),
    main_rule=lambda T, g: Or(List(g.num, list_cls=T.Example.list),
                              T.ListNode(g.example)))

print('Done')
