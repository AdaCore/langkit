"""
Test that grammars which are troublesome for the unparsing machinery are
properly detected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Or, Pick

from lexer_example import Token
from utils import emit_and_print_errors


def run(name, **kwargs):

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Root(FooNode):
        f = Field()

    class Identifier(FooNode):
        token_node = True

    class Number(FooNode):
        token_node = True

    class Nodes(object):
        pass
    Nodes.Root = Root
    Nodes.Identifier = Identifier
    Nodes.Number = Number

    g = Grammar('main_rule')
    g.add_rules(**{name: parser_fn(Nodes, g)
                   for name, parser_fn in kwargs.items()})
    emit_and_print_errors(g, generate_unparser=True)
    print('')


run(
    'Pick in Or',
    main_rule=lambda T, g: T.Root(
        Or(
            Pick('example', T.Identifier(Token.Identifier)),
            T.Number(Token.Number)
        )
    )
)
run(
    'Toplevel Pick',
    main_rule=lambda T, g: Pick('example', T.Root(g.item)),
    item=lambda T, g: Or(T.Identifier(Token.Identifier),
                         T.Number(Token.Number)),
)
run(
    'Several token kinds for token node (1)',
    main_rule=lambda T, g: T.Root(
        Or(
            T.Identifier(Token.Identifier() | Token.Example()),
            T.Number(Token.Number)
        )
    )
)
run(
    'Several token kinds for token node (2)',
    main_rule=lambda T, g: T.Root(
        Or(
            T.Identifier(Token.Identifier),
            T.Identifier(Token.Example),
            T.Number(Token.Number)
        )
    )
)
run(
    'Pick in List',
    main_rule=lambda T, g: List(
        'example',
        T.Root(
            Or(
                Pick('example', T.Identifier(Token.Identifier)),
                T.Number(Token.Number)
            )
        )
    )
)
print('Done')
